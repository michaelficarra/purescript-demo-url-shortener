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

database :: Database
database = selectDb 2

indexHandler :: Handler
indexHandler = do
  sendFile "./index.html"

app :: App
app = do
  liftEff $ Console.log "Initialising server"
  get "/" indexHandler

main = do
  Console.log "Starting server"
  listenHttp app port \_ ->
    Console.log ("Listening at " <> linkColour ("http://localhost:" <> show port))
