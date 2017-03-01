module DOM.QueryEmail (readEmail) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Foreign (Foreign, readString)


foreign import queryEmail :: forall eff . Eff (dom :: DOM | eff) Foreign


readEmail :: forall eff . Eff (dom :: DOM | eff) (Maybe String)
readEmail = do
  val <- queryEmail
  let str = readString val
  case runExcept str of
    Left err -> pure Nothing
    Right email ->  pure $ Just email
