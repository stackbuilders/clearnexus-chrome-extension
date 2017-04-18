module DOM.Listener (textAreaListener, CLEANTIMER) where


import Prelude
import Config (CHROME)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)
import DOM.QueryDocument (pasteLink, queryEmail)
import Data.Either (Either(..))
import Data.Function.Eff (EffFn1, mkEffFn1, runEffFn1)
import Data.Maybe (Maybe(..))
import GenerateClient.Types (EmailProperties(..), LinkData(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (AjaxError(..), ErrorDescription(..), errorToString)
import Util (getLink, getSubscriptionStatus, postNewLink)


type Items r = { authtoken :: String | r }


foreign import data CLEANTIMER :: !

-- << Type EffFn1 is necessary due to compilation issues of callbacks with effects
foreign import getStoredToken :: forall eff r .
                                 EffFn1 (ajax :: AJAX, chrome :: CHROME | eff)
                                        (EffFn1 (ajax :: AJAX, chrome :: CHROME | eff) (Items r) Unit)
                                        Unit

foreign import safeSetTimeout :: forall eff .
                                 EffFn1 (cleantimer :: CLEANTIMER | eff)
                                        (EffFn1 (cleantimer :: CLEANTIMER | eff) Unit Unit)
                                        Unit


reqCallback :: forall eff r .
               String
            -> String
            -> Event
            -> Items r
            -> Eff ( dom :: DOM
                   , console :: CONSOLE
                   , timer :: TIMER
                   , alert :: ALERT
                   , ajax :: AJAX
                   , cleantimer :: CLEANTIMER
                   , chrome :: CHROME | eff ) Unit
reqCallback serverUrl email event items = do
  runAff logShow (const $ pure unit) (runExceptT asyncGetEmailProps >>= (runExceptT <<< asyncPostLink))
  pure unit
  where
    -- << >> --
    displayAlert msg = liftEff window >>= (liftEff <<< alert msg)
    -- << >> --
    asyncGetEmailProps = do
      (EmailProperties props) <- ExceptT $ getSubscriptionStatus serverUrl email items.authtoken
      (LinkData lData) <- ExceptT $ getLink serverUrl props.link_token items.authtoken
      if props.subscribed
        then do
          liftEff $ pasteLink Nothing lData.unsubscription_link
          liftEff $ textAreaListener serverUrl email event
        else  do
          displayAlert "Email is UNSUBSCRIBED"
          liftEff $ textAreaListener serverUrl email event
    -- << >> --
    asyncPostLink (Left ajaxErr@(AjaxError err)) =
      case err.description of
        UnexpectedHTTPStatus  { status: StatusCode 401 } -> do
          displayAlert "Please provide auth token"
        UnexpectedHTTPStatus  { status: StatusCode 404 } -> do
          (LinkData lData) <- ExceptT $ postNewLink serverUrl email items.authtoken
          liftEff $ pasteLink Nothing lData.unsubscription_link
          liftEff $ textAreaListener serverUrl email event
        _ -> liftEff $ logShow $ errorToString ajaxErr
    asyncPostLink _ = pure unit


-- << Listener for events in the <textarea name="to"> element
textAreaListener :: forall eff .
                    String
                 -> String
                 -> Event
                 -> Eff ( dom :: DOM
                        , console :: CONSOLE
                        , alert :: ALERT
                        , timer :: TIMER
                        , cleantimer :: CLEANTIMER
                        , ajax :: AJAX
                        , chrome :: CHROME | eff ) Unit
textAreaListener serverUrl lastEmail event = do
  maybeEmail <- queryEmail
  case maybeEmail of
    Nothing -> do
      -- Use safeSettimeout not to block the browser and not to accumulate callbacks
      -- in the background.
      runEffFn1 safeSetTimeout tmOutCallback
    Just email -> do
      if email == lastEmail
        then do
          runEffFn1 safeSetTimeout tmOutCallback
        else do
          let callback = mkEffFn1 $ reqCallback serverUrl email event
          runEffFn1 getStoredToken callback
  where
    tmOutCallback = mkEffFn1 $ \_ -> textAreaListener serverUrl lastEmail event
