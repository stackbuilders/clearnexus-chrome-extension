module DOM.Listener (textAreaListener) where


import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)
import DOM.QueryDocument (queryDocElt, readEmails)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Function.Eff (EffFn1, mkEffFn1, runEffFn1)
import Data.Maybe (Maybe(..))
import GenerateClient.Types (EmailProperties(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError(..))
import Util (getSubscriptionStatus)


-- << Type EffFn1 is necessary due to compilation issues of callbacks with effects
foreign import getStoredToken :: forall eff r .
                                 EffFn1 (ajax :: AJAX | eff)
                                        (EffFn1 (ajax :: AJAX | eff) { authtoken :: String | r } Unit)
                                        Unit


clearnexusUrl :: String
clearnexusUrl = "http://localhost:8000/"


reqCallback :: forall eff r .
               String
            -> String
            -> { authtoken :: String | r }
            -> Eff ( dom :: DOM
                   , console :: CONSOLE
                   , timer ∷ TIMER
                   , alert :: ALERT
                   , ajax :: AJAX | eff ) Unit
reqCallback serverUrl email items = do
  runAff logShow successCallback (getSubscriptionStatus serverUrl email items.authtoken)
  pure unit
  where
    successCallback (Left (AjaxError obj)) = do
      win <- window
      alert "Register a valid ClearNexus token!" win
    successCallback (Right (EmailProperties obj)) = do
      win <- window
      if obj.subscribed
        then alert "This user is subscribed!" win
        else alert "This user is NOT subscribed..." win


-- << Listener for events in the <textarea name="to"> element
textAreaListener :: forall eff .
                    Event
                 -> Eff ( dom :: DOM
                        , console :: CONSOLE
                        , alert :: ALERT
                        , timer :: TIMER
                        , ajax :: AJAX | eff ) Unit
textAreaListener event = do
  maybeElt <- queryDocElt "div[class=vR]"
  case maybeElt of
    Nothing -> do
      -- Poll events every second not to block the browser
      setTimeout 1000 (textAreaListener event)
      pure unit
    Just textArea ->
      addEventListener (EventType "dblclick")
                       (eventListener $ emailListener)
                       false
                       textArea
  where
    -- Listener for emails in textarea
    emailListener evt = do
      win <- window
      emails <- readEmails Nothing
      case head emails of
        Nothing -> do
          -- Poll events every 500 mls not to block the browser
          setTimeout 500 $ emailListener evt
          pure unit
        Just email -> do
          let callback = mkEffFn1 $ reqCallback clearnexusUrl email
          runEffFn1 getStoredToken callback
