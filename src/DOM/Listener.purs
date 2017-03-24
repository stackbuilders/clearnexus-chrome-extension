module DOM.Listener (textAreaListener) where


import Prelude
import Config (Config(..), ChromeEff)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
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
import GenerateClient.Types (EmailProperties(..), LinkData(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (AjaxError(..), ErrorDescription(..), errorToString)
import Util (getLink, getSubscriptionStatus, postNewLink)


-- << Type EffFn1 is necessary due to compilation issues of callbacks with effects
foreign import getStoredToken :: forall eff r .
                                 EffFn1 (ajax :: AJAX, chrome :: ChromeEff | eff)
                                        (EffFn1 (ajax :: AJAX, chrome :: ChromeEff | eff) { authtoken :: String | r } Unit)
                                        Unit


reqCallback :: forall eff r .
               String
            -> String
            -> { authtoken :: String | r }
            -> Eff ( dom :: DOM
                   , console :: CONSOLE
                   , timer ∷ TIMER
                   , alert :: ALERT
                   , ajax :: AJAX
                   , chrome :: ChromeEff | eff ) Unit
reqCallback serverUrl email items = do
  runAff logShow (const $ pure unit) (runExceptT asyncGetEmailProps >>= (runExceptT <<< asyncPostLink))
  pure unit
  where
    asyncGetEmailProps = do
      (EmailProperties props) <- ExceptT $ getSubscriptionStatus serverUrl email items.authtoken
      (LinkData lData) <- ExceptT $ getLink serverUrl props.link_token items.authtoken
      let msg = if props.subscribed
                then "User is subscribed: " <> lData.unsubscription_link
                else "User is NOT subscribed: " <> lData.unsubscription_link
      win <- liftEff window
      liftEff $ alert msg win
    -- << >> --
    asyncPostLink (Left ajaxErr@(AjaxError err)) =
      case err.description of
        UnexpectedHTTPStatus obj ->
          if obj.status == StatusCode 404
          then do
            (LinkData lData) <- ExceptT $ postNewLink serverUrl email items.authtoken
            let msg = "Link created for the first time: " <> lData.unsubscription_link
            win <- liftEff window
            liftEff $ alert msg win
          else liftEff $ logShow $ errorToString ajaxErr
        _ -> liftEff $ logShow $ errorToString ajaxErr
    asyncPostLink _ = pure unit

-- << Listener for events in the <textarea name="to"> element
textAreaListener :: forall eff .
                    Config
                 -> Event
                 -> Eff ( dom :: DOM
                        , console :: CONSOLE
                        , alert :: ALERT
                        , timer :: TIMER
                        , ajax :: AJAX
                        , chrome :: ChromeEff | eff ) Unit
textAreaListener (Config conf) event = do
  maybeElt <- queryDocElt "div[class=vR]"
  case maybeElt of
    Nothing -> do
      -- Poll events every second not to block the browser
      setTimeout 1000 (textAreaListener (Config conf) event)
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
          let callback = mkEffFn1 $ reqCallback conf.url email
          runEffFn1 getStoredToken callback
