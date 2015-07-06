module UrlShortener.Redis where

import Control.Monad.Eff (Eff())
import Data.Char (fromCharCode)
import Data.String (fromChar)
import Data.Maybe (Maybe(Just, Nothing))

foreign import data Database :: *
foreign import data DatabaseEffect :: !
type DBEff r a = Eff (databaseEffect :: DatabaseEffect | r) a

foreign import selectDb """
  function selectDb(n) {
    var db = require('redis').createClient();
    db.select(n);
    return db;
  }
  """ :: Number -> Database

meta = selectDb 1
database = selectDb 2

intToKey :: Number -> String
intToKey i | i < 26 = fromChar $ fromCharCode (i + 97)
intToKey i | i < 52 = fromChar $ fromCharCode (i - 26 + 65)
intToKey i = intToKey ((i / 52) - 1) <> intToKey (i % 52)

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

foreign import sampleLinks """
  function sampleLinks(n){
  return function(db) {
  return function(cb) {
    return function() {
      console.log('scan 0 count ' + n);
      db.scan(0, 'count', n, function(err, response) {
        var shortNames = response[1];
        db.mget(shortNames, function(err, urls) {
          console.log('mget ' + shortNames.join(' '));
          var result = Object.create(null);
          for (var i = 0, l = shortNames.length; i < l; ++i) {
            result[shortNames[i]] = urls[i];
          }
          cb(result)();
        });
      });
    };
  };};}
  """ :: forall eff r. Number -> Database -> ({| r} -> DBEff eff Unit) -> DBEff eff Unit
