module UrlShortener where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import qualified Control.Monad.Eff.Console as Console
import Data.String (toUpper)
import Node.Express.App (get, listenHttp, post, setProp, use, useExternal, App())
import Node.Express.Handler (getBodyParam, getHostname, getMethod, getOriginalUrl, getRouteParam, next, redirect, sendFile, sendJson, setStatus, Handler())
import Prelude ((<<<), (<>), ($), bind, show)

import UrlShortener.Express (bodyParser, liftCallback)
import UrlShortener.Redis (insert, lookup, nextAvailableKey, sampleLinks, selectDb, Database())

foreign import port :: Int

database :: Database
database = selectDb 2

logger :: Handler
logger = do
  url <- getOriginalUrl
  maybeMethod <- getMethod
  let method = maybe "" ((<> " ") <<< toUpper <<< show) maybeMethod
  liftEff $ Console.log (">>> " <> method <> url)
  next

indexHandler :: Handler
indexHandler = do
  sendFile "./index.html"

listHandler :: Handler
listHandler = do
  liftCallback (sampleLinks 5 database) sendJson

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
      liftEff $ Console.error "ERROR: shortName parameter not found"
      next
    Just shortName ->
      liftCallback (lookup shortName database) $ \maybeUrl -> do
        maybe (setStatus 404) redirect maybeUrl
        next

app :: App
app = do
  liftEff $ Console.log "Starting server"
  setProp "json spaces" 2.0
  use logger
  useExternal bodyParser
  get "/" indexHandler
  get "/_list" listHandler
  post "/_shorten" shortenHandler
  get "/:shortName" redirectHandler

main = do
  Console.log "Setting up"
  listenHttp app port \_ ->
    Console.log ("Listening on " <> show port)
