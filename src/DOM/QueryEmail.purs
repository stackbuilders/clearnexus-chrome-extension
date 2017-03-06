module DOM.QueryEmail ( readEmails, DocElt ) where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readString)
import Data.Foreign.Null (Null(..))
import Data.Traversable (traverse)
import Data.Maybe (Maybe)


type DocElt = { getElementsByClassName ::
                   String -> Array { firstChild :: { getAttribute :: String -> String } }
              }


foreign import queryEmails :: forall eff . Null DocElt -> Eff (dom :: DOM | eff) Foreign


-- << When used in browser Do Not provide DocElt (Nothing). In tests, inject Just DocElt
readEmails :: forall eff .  Maybe DocElt -> Eff (dom :: DOM | eff) (Array String)
readEmails mock = do
  query <- queryEmails (Null mock) -- provide the mock
  values <- pure $ either (const []) id (runExcept $ readArray query)
  emails <- pure $ either (const []) id (runExcept $ traverse readString values)
  pure emails
