module UrlShortener.Express where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.Function (Fn3())
import Node.Express.Handler (capture, next, HandlerM())
import Node.Express.Types (ExpressM(), Response(), Request())
import Prelude (bind, Unit())

foreign import bodyParser :: Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

liftCallback :: forall a eff. ((a -> Eff eff Unit) -> Eff eff Unit) -> (a -> HandlerM Unit) -> HandlerM Unit
liftCallback f next = do
  cb <- capture next
  liftEff (f cb)
