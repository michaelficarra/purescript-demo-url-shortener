module UrlShortener where

import Prelude ((<<<), (<>), (<$>), ($), show, Unit(), Ord)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.String (toUpper)
import Debug.Trace (trace)

import Node.Express.Types ()
import Node.Express.App (get, listenHttp, post, setProp, use, App())
import Node.Express.Handler (capture, getMethod, getOriginalUrl, getRouteParam, next, redirect, sendFile, sendJson, setStatus, Handler(), HandlerM())


foreign import data Database :: *
foreign import data DatabaseEffect :: !
type DBEff r a = Eff (databaseEffect :: DatabaseEffect | r) a

liftCallback :: forall a eff. ((a -> Eff eff Unit) -> Eff eff Unit) -> (a -> HandlerM Unit) -> HandlerM Unit
liftCallback f next = do
  cb <- capture next
  liftEff (f cb)

foreign import database "var database = require('redis').createClient();" :: Database

foreign import insert """
  function insert(key) {
  return function(val) {
  return function(db) {
  return function(cb) {
    return function(){
      db.set(key, val, 'nx', function(err, val){ cb(err == null && val != null); });
    }
  };};}
  """ :: forall eff. String -> String -> Database -> (Boolean -> DBEff eff Unit) -> DBEff eff Unit

foreign import lookupP """
  function lookupP(Nothing) {
  return function(Just) {
  return function(key) {
  return function(db) {
  return function(cb) {
    return function(){
      db.get(key, function(err, val){ cb(err == null && val != null ? Just(val) : Nothing)(); });
    };
  };};};};}
  """ :: forall a eff. Maybe a -> (String -> Maybe String) -> String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit

lookup :: forall eff. String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit
lookup = lookupP Nothing Just

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
  sendJson {
    google: "http://google.com",
    mozilla: "http://mozilla.org",
    fedora: "http://fedoraproject.org"
  }

redirectHandler :: Handler
redirectHandler = do
  maybeShortName <- getRouteParam "shortName"
  case maybeShortName of
    Nothing -> do
      liftEff $ trace "ERROR: shortName parameter not found"
    Just shortName -> do
      liftCallback (lookup shortName database) $ \maybeUrl -> do
        maybe (setStatus 404) redirect maybeUrl

app :: App
app = do
  liftEff $ trace "Starting server"
  setProp "json spaces" 2
  use logger
  get "/" indexHandler
  get "/list" listHandler
  get "/:shortName" redirectHandler

foreign import port "var port = process.env.PORT || 8080" :: Number

main = do
  trace "Setting up"
  listenHttp app port \_ ->
    trace ("Listening on " <> show port)
