module DOM.QueryDocument ( readEmails, DocElt ) where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readString)
import Data.Foreign.Null (Null(..))
import Data.Traversable (traverse)
import Data.Maybe (Maybe)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (Element, documentToParentNode)
import Data.Nullable (toMaybe)



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

-- << Get the <textarea name="to"> tag in the Gmail document
queryTextArea :: forall eff . Eff (dom :: DOM | eff) (Maybe Element)
queryTextArea = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  toMaybe <$> querySelector "textarea[name=to]" (documentToParentNode doc)
