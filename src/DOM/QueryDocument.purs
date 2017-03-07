module DOM.QueryDocument ( readEmails
                         , queryGmailElt
                         , textAreaListener
                         , composeBtnListener
                         , clickEvent
                         , delayExtInjection
                         , DocElt      ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventTarget, EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (ALERT, htmlDocumentToDocument)
import DOM.HTML.Window (alert, document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (documentToParentNode, elementToEventTarget)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readString)
import Data.Foreign.Null (Null(..))
import Data.Maybe (Maybe(..))
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


-- << Query a Gmail doc element acording to a query-selector
queryGmailElt :: forall eff . String -> Eff (dom :: DOM | eff) (Maybe EventTarget)
queryGmailElt selector = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  elt <-toMaybe <$> querySelector selector (documentToParentNode doc)
  pure $ elementToEventTarget <$> elt


-- << Listener for events in the <textarea name="to"> element
textAreaListener :: forall eff . Event
                              -> Eff (dom :: DOM, alert ∷ ALERT | eff) Unit
textAreaListener evt = do
  win <- window
  alert "You input a new email!" win


-- << Listener for events in the <div role="button" gh="cm" > element
composeBtnListener :: forall eff . Event
                                -> Eff (dom :: DOM, alert ∷ ALERT | eff) Unit
composeBtnListener evt = do
  win <- window
  alert "You pressed the Compose Btn!" win


-- << Wrapper to delay the injection of our extension's JavaScript code until a
-- << specific element has been found.
delayExtInjection :: forall eff . String
                               -> EventType
                               -> (Event -> Eff (dom :: DOM, timer :: TIMER | eff) Unit)
                               -> Eff (dom :: DOM, timer :: TIMER | eff) Unit
delayExtInjection query evetType listener = do
  loop query
  where
    loop q = do
      maybeElt <- queryGmailElt q
      case maybeElt of
        Nothing -> do
          setTimeout 1000 (loop q)
          pure unit
        Just elt ->
          addEventListener clickEvent
                           (eventListener listener)
                           false
                           elt


-- << We are going to listen to *keypress* events
clickEvent :: EventType
clickEvent = EventType "click"
