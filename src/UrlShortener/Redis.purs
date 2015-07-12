module UrlShortener.Redis where

import Control.Monad.Eff (Eff())
import Data.Char (fromCharCode)
import Data.String (fromChar)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude ((<), (<<<), ($), (+), (/), (-), (<>), div, mod, Unit())

foreign import data Database :: *
foreign import data DatabaseEffect :: !

type DBEff r a = Eff (databaseEffect :: DatabaseEffect | r) a

foreign import insert :: forall eff. String -> String -> Database -> (Boolean -> DBEff eff Unit) -> DBEff eff Unit
foreign import lookupP :: forall a eff. Maybe a -> (String -> Maybe String) -> String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit
foreign import nextAvailableKeyP :: forall eff. Database -> (Int -> DBEff eff Unit) -> DBEff eff Unit
foreign import sampleLinks :: forall eff r. Int -> Database -> ({| r} -> DBEff eff Unit) -> DBEff eff Unit
foreign import selectDb :: Int -> Database

metadata = selectDb 1

nextAvailableKey :: forall eff. (String -> DBEff eff Unit) -> DBEff eff Unit
nextAvailableKey cb = nextAvailableKeyP metadata (cb <<< intToKey)
  where
  intToKey :: Int -> String
  intToKey i | i < 26 = fromChar $ fromCharCode (i + 97)
  intToKey i | i < 52 = fromChar $ fromCharCode (i - 26 + 65)
  intToKey i = intToKey ((div i 52) - 1) <> intToKey (mod i 52)

lookup :: forall eff. String -> Database -> (Maybe String -> DBEff eff Unit) -> DBEff eff Unit
lookup = lookupP Nothing Just
