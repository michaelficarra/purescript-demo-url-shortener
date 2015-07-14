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
foreign import linkColour :: String -> String
foreign import httpMethodColour :: String -> String

database :: Database
database = selectDb 2

logger :: Handler
logger = do
  url <- getOriginalUrl
  maybeMethod <- getMethod
  let method = maybe "" ((<> " ") <<< httpMethodColour <<< toUpper <<< show) maybeMethod
  liftEff $ Console.log (">>> " <> method <> url)
  next

indexHandler :: Handler
indexHandler = do
  sendFile "./index.html"

listHandler :: Handler
listHandler = do
  liftCallback (sampleLinks 5 database) sendJson

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
  liftEff $ Console.log "Initialising server"
  use logger
  get "/" indexHandler
  get "/_list" listHandler
  get "/:shortName" redirectHandler

main = do
  Console.log "Starting server"
  listenHttp app port \_ ->
    Console.log ("Listening at " <> linkColour ("http://localhost:" <> show port))
