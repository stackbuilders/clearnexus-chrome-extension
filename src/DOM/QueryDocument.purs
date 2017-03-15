module DOM.QueryDocument ( readEmails
                         , queryDocElt
                         , delayExtInjection
                         , DocElt      ) where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventTarget, EventType)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
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
queryDocElt :: forall eff . String -> Eff (dom :: DOM | eff) (Maybe EventTarget)
queryDocElt selector = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  elt <- toMaybe <$> querySelector selector (documentToParentNode doc)
  pure $ elementToEventTarget <$> elt


-- << Wrapper to delay the injection of our extension's JavaScript code until a
-- << specific element has been found.
delayExtInjection :: forall eff .
                     String
                  -> EventType
                  -> (Event -> Eff (dom :: DOM, timer :: TIMER | eff) Unit)
                  -> Eff (dom :: DOM, timer :: TIMER | eff) Unit
delayExtInjection query eventType listener = do
  loop query
  where
    loop q = do
      maybeElt <- queryDocElt q
      case maybeElt of
        Nothing -> do
          setTimeout 1000 (loop q)
          pure unit
        Just elt ->
          addEventListener eventType
                           (eventListener listener)
                           false
                           elt
