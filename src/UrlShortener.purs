module UrlShortener where

import Prelude ((<<<), (<>), (<$>), ($), (<), (%), (/), (+), (-), show, Unit(), Ord)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Function (Fn3())
import Data.String (toUpper)
import Debug.Trace (trace)
import Node.Express.Types (ExpressM(), Response(), Request())
import Node.Express.App (get, listenHttp, post, setProp, use, useExternal, App())
import Node.Express.Handler (capture, getBodyParam, getHostname, getMethod, getOriginalUrl, getRouteParam, next, redirect, send, sendFile, sendJson, setStatus, Handler(), HandlerM())

import UrlShortener.Redis (insert, lookup, nextAvailableKey, sampleLinks, selectDb)

liftCallback :: forall a eff. ((a -> Eff eff Unit) -> Eff eff Unit) -> (a -> HandlerM Unit) -> HandlerM Unit
liftCallback f next = do
  cb <- capture next
  liftEff (f cb)

foreign import bodyParser """
  var bodyParser = require('body-parser').urlencoded({extended: false});
  """ :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

database = selectDb 2

logger :: Handler
logger = do
  url <- getOriginalUrl
  maybeMethod <- getMethod
  let method = maybe "" ((<> " ") <<< toUpper <<< show) maybeMethod
  liftEff $ trace (">>> " <> method <> url)
  next

indexHandler :: Handler
indexHandler = do
  sendFile "./index.html"

listHandler :: Handler
listHandler = do
  liftCallback (sampleLinks 5 database) $ \linksRecord ->
    sendJson linksRecord

shortenHandler :: Handler
shortenHandler = do
  maybeUrl <- getBodyParam "url"
  case maybeUrl of
    Nothing -> do
      setStatus 400
      sendJson { error: true, message: "no URL parameter found" }
    Just url -> do
      liftCallback nextAvailableKey $ \shortName ->
        liftCallback (insert shortName url database) $ \success ->
          if success then do
            setStatus 201
            host <- getHostname
            -- NOTE: host is provided by the client and should not be trusted
            sendJson {
              message: "success",
              shortUrl: "http://" <> host <> ":" <> show port <> "/" <> shortName,
              url: url
            }
          else do
            setStatus 500
            sendJson { error: true, message: "unable to store URL for key " <> shortName }

redirectHandler :: Handler
redirectHandler = do
  maybeShortName <- getRouteParam "shortName"
  case maybeShortName of
    Nothing -> do
      setStatus 404
      liftEff $ trace "ERROR: shortName parameter not found"
      next
    Just shortName ->
      liftCallback (lookup shortName database) $ \maybeUrl -> do
        maybe (setStatus 404) redirect maybeUrl
        next

app :: App
app = do
  liftEff $ trace "Starting server"
  setProp "json spaces" 2
  use logger
  useExternal bodyParser
  get "/" indexHandler
  get "/_list" listHandler
  post "/_shorten" shortenHandler
  get "/:shortName" redirectHandler

foreign import port "var port = process.env.PORT || 8080" :: Number

main = do
  trace "Setting up"
  listenHttp app port \_ ->
    trace ("Listening on " <> show port)
