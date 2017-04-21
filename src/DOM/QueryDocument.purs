module DOM.QueryDocument ( pasteLink
                         , queryDocElt
                         , queryEmail
                         , delayExtInjection
                         , DocumentElement  ) where


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
import DOM.Node.Element (getAttribute)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (documentToParentNode, elementToEventTarget)
import Data.Either (either)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Null (Null(..), unNull, readNull)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)


type DocumentElement = { getElementsByClassName ::
                            String -> Array { firstChild :: { getAttribute :: String -> String } }
                       , querySelector :: String -> Maybe {
                              childNodes :: Array Unit
                            , appendChild :: Unit -> Unit
                            , insertBefore :: Unit -> Unit
                            }
                       , createElement :: String -> {
                            innerText :: String
                          , href :: String
                          }
                       }


foreign import uncurriedPasteLink :: forall eff .
                                     Fn2 (Null DocumentElement)
                                         String
                                         (Eff (dom :: DOM | eff) Foreign)


-- << Paste a link in the Gmail's compose box
pasteLink :: forall eff .
             Maybe DocumentElement
          -> String
          -> Eff ( dom :: DOM | eff) (Maybe String)
pasteLink optDoc link = do
  value <- curried (Null optDoc) link
  let eitherDoc = unNull <$> runExcept (readNull readString value)
  -- Returns link for create tests
  either (const $ pure Nothing) pure eitherDoc
  where
    curried = runFn2 uncurriedPasteLink


-- << Query a Gmail doc element acording to a query-selector
queryDocElt :: forall eff . String -> Eff (dom :: DOM | eff) (Maybe EventTarget)
queryDocElt selector = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  elt <- toMaybe <$> querySelector selector (documentToParentNode doc)
  pure $ elementToEventTarget <$> elt


-- << Query an email from the Gmail's compose box
queryEmail :: forall eff . Eff (dom :: DOM | eff) (Maybe String)
queryEmail = do
  win <- window
  doc <- htmlDocumentToDocument <$> document win
  maybeElt <- toMaybe <$> querySelector "div.vR span" (documentToParentNode doc)
  case maybeElt of
    Just elt -> toMaybe <$> getAttribute "email" elt
    _ -> pure Nothing


-- << Wrapper to delay the injection of our extension's JavaScript code until a
--    specific element has been found.
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
