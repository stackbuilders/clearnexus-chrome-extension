module DOM.Listener (textAreaListener) where


import Prelude
import Config (Config(..), CHROME)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)
import DOM.QueryDocument (readEmails, pasteLink)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Eff (EffFn1, mkEffFn1, runEffFn1)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))
import GenerateClient.Types (EmailProperties(..), LinkData(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (AjaxError(..), ErrorDescription(..), errorToString)
import Util (getLink, getSubscriptionStatus, postNewLink)


type Items r = { authtoken :: String | r }

-- << Type EffFn1 is necessary due to compilation issues of callbacks with effects
foreign import getStoredToken :: forall eff r .
                                 EffFn1 (ajax :: AJAX, chrome :: CHROME | eff)
                                        (EffFn1 (ajax :: AJAX, chrome :: CHROME | eff) (Items r) Unit)
                                        Unit

foreign import uncurriedAddEventListener :: forall eff .
                                            Fn4 String
                                                String
                                                (Eff (dom :: DOM | eff) Unit)
                                                Boolean
                                                (Eff (dom :: DOM | eff) Foreign)

reqCallback :: forall eff r .
               String
            -> String
            -> Items r
            -> Eff ( dom :: DOM
                   , console :: CONSOLE
                   , timer ∷ TIMER
                   , alert :: ALERT
                   , ajax :: AJAX
                   , chrome :: CHROME | eff ) Unit
reqCallback serverUrl email items = do
  runAff logShow (const $ pure unit) (runExceptT asyncGetEmailProps >>= (runExceptT <<< asyncPostLink))
  pure unit
  where
    displayAlert msg = liftEff window >>= (liftEff <<< alert msg)
    -- << >> --
    asyncGetEmailProps = do
      (EmailProperties props) <- ExceptT $ getSubscriptionStatus serverUrl email items.authtoken
      (LinkData lData) <- ExceptT $ getLink serverUrl props.link_token items.authtoken
      if props.subscribed
        then do
          liftEff $ pasteLink Nothing lData.unsubscription_link
          pure unit
        else  displayAlert "Email is UNSUBSCRIBED"
    -- << >> --
    asyncPostLink (Left ajaxErr@(AjaxError err)) =
      case err.description of
        UnexpectedHTTPStatus  { status: StatusCode 401 } -> do
          displayAlert "Please provide auth token"
        UnexpectedHTTPStatus  { status: StatusCode 404 } -> do
          (LinkData lData) <- ExceptT $ postNewLink serverUrl email items.authtoken
          liftEff $ pasteLink Nothing lData.unsubscription_link
          pure unit
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
                        , chrome :: CHROME | eff ) Unit
textAreaListener (Config conf) event = do
  runFn4 uncurriedAddEventListener "div[class=vR]" "dblclick" emailListener false
  setTimeout 500 $ textAreaListener (Config conf) event
  pure unit

-- << Listener for emails in textarea
emailListener :: forall eff . 
                 Eff ( dom ∷ DOM 
                     , timer ∷ TIMER 
                     , ajax ∷ AJAX 
                     , chrome ∷ CHROME 
                     , console ∷ CONSOLE 
                     , alert ∷ ALERT | eff ) Unit
emailListener = do
  win <- window
  emails <- readEmails Nothing
  case head emails of
    Nothing -> do
      -- Poll events every 500 mls not to block the browser
      setTimeout 500 $ emailListener
      pure unit
    Just email -> do
      let callback = mkEffFn1 $ reqCallback "https://staging.clearnex.us/" email
      runEffFn1 getStoredToken callback
      pure unit
