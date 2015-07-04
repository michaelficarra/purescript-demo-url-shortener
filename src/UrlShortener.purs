module UrlShortener where

import Prelude ((<<<), (<>), (<$>), ($), (<), (%), (/), (+), (-), show, Unit(), Ord)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Char (fromCharCode)
import Data.Function (Fn3())
import Data.String (fromChar, toUpper)
import Debug.Trace (trace)

import Node.Express.Types (ExpressM(), Response(), Request())
import Node.Express.App (get, listenHttp, post, setProp, use, useExternal, App())
import Node.Express.Handler (capture, getBodyParam, getHostname, getMethod, getOriginalUrl, getRouteParam, next, redirect, send, sendFile, sendJson, setStatus, Handler(), HandlerM())


foreign import data Database :: *
foreign import data DatabaseEffect :: !
type DBEff r a = Eff (databaseEffect :: DatabaseEffect | r) a

liftCallback :: forall a eff. ((a -> Eff eff Unit) -> Eff eff Unit) -> (a -> HandlerM Unit) -> HandlerM Unit
liftCallback f next = do
  cb <- capture next
  liftEff (f cb)

intToKey :: Number -> String
intToKey i | i < 26 = fromChar $ fromCharCode (i + 97)
intToKey i | i < 52 = fromChar $ fromCharCode (i - 26 + 65)
intToKey i = intToKey ((i / 52) - 1) <> intToKey (i % 52)

foreign import meta "var meta = require('redis').createClient(); meta.select(1);" :: Database
foreign import database "var database = require('redis').createClient(); database.select(2);" :: Database

foreign import nextAvailableKeyP """
  function nextAvailableKeyP(db) {
  return function(cb) {
    return function(){
      console.log('incr count');
      db.incr('count', function(err, val){
        cb(val - 1)();
      });
    };
  };}
  """ :: forall eff. Database -> (Number -> DBEff eff Unit) -> DBEff eff Unit

nextAvailableKey :: forall eff. (String -> DBEff eff Unit) -> DBEff eff Unit
nextAvailableKey cb = nextAvailableKeyP meta (cb <<< intToKey)

foreign import insert """
  function insert(key) {
  return function(val) {
  return function(db) {
  return function(cb) {
    return function(){
      console.log('set ' + key + ' ' + val);
      db.set(key, val, 'nx', function(err, ok){
        cb(err == null && ok != null)();
      });
    }
  };};};}
  """ :: forall eff. String -> String -> Database -> (Boolean -> DBEff eff Unit) -> DBEff eff Unit

foreign import lookupP """
  function lookupP(Nothing) {
  return function(Just) {
  return function(key) {
  return function(db) {
  return function(cb) {
    return function(){
      console.log('get ' + key);
      db.get(key, function(err, val){
        cb(err == null && val != null ? Just(val) : Nothing)();
      });
    };
  };};};};}
  """ :: forall a eff. Maybe a -> (String -> Maybe String) -> String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit

lookup :: forall eff. String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit
lookup = lookupP Nothing Just

foreign import bodyParser """
  var bodyParser = require('body-parser').urlencoded({extended: false});
  """ :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

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
    google: "https://google.com",
    mozilla: "http://mozilla.org",
    fedora: "http://fedoraproject.org"
  }

shortenHandler :: Handler
shortenHandler = do
  maybeUrl <- getBodyParam "url"
  case maybeUrl of
    Nothing -> do
      setStatus 400
      send "ERROR: no URL parameter found"
    Just url -> do
      liftCallback nextAvailableKey $ \shortName ->
        liftCallback (insert shortName url database) $ \success ->
          if success then do
            setStatus 201
            host <- getHostname
            send ("Success: " <> "http://" <> host <> ":" <> show port <> "/" <> shortName)
          else do
            setStatus 500
            send ("ERROR: unable to store URL for key " <> shortName)

redirectHandler :: Handler
redirectHandler = do
  maybeShortName <- getRouteParam "shortName"
  case maybeShortName of
    Nothing -> do
      setStatus 404
      send "ERROR: shortName parameter not found"
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
