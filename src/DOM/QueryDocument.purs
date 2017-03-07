module DOM.QueryDocument ( readEmails
                         , queryTextArea
                         , extensionListener
                         , keyPressEvt
                         , DocElt      ) where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Types (Event, EventTarget, EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (alert, document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (documentToParentNode, elementToEventTarget)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readString)
import Data.Foreign.Null (Null(..))
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)


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

-- << Get the <textarea name="to"> tag in the Gmail document as an EventTarget
queryTextArea :: forall eff . Eff (dom :: DOM | eff) (Maybe EventTarget)
queryTextArea = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  elt <-toMaybe <$> querySelector "textarea[name=to]" (documentToParentNode doc)
  pure $ elementToEventTarget <$> elt

-- << Listener for events in the <textarea name="to"> element
extensionListener :: forall eff . Event
                               -> Eff (dom :: DOM, alert ∷ ALERT | eff) Unit
extensionListener evt = do
  win <- window
  alert "You input a new email!" win

-- << We are going to listen to *keypress* events
keyPressEvt :: EventType
keyPressEvt = EventType "keypress"
