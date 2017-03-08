module DOM.Listener (clickEvent, textAreaListener) where


import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import DOM (DOM)
import DOM.Event.Types (Event, EventTarget, EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)
import DOM.QueryDocument (queryDocElt)
import Data.Either (Either(..))
import GenerateClient.Types (EmailProperties(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError(..))
import Util (getSubscriptionStatus)



-- << Events
clickEvent :: EventType
clickEvent = EventType "click"
-- >>


-- << Listener for events in the <textarea name="to"> element
-- << This is provisional code for testing purposes.
textAreaListener :: forall eff . Event
                              -> Eff ( dom :: DOM
                                     , console :: CONSOLE
                                     , alert :: ALERT
                                     , ajax :: AJAX | eff) Unit
textAreaListener evt = do
  runAff logShow
         successCallback
         (getSubscriptionStatus "https://staging.clearnex.us/"
                                "subscribed@clearnex.us"
                                "TEST-TOKEN")
  pure unit
  where
    successCallback (Left (AjaxError obj )) = logShow "There was an error"
    successCallback (Right (EmailProperties obj)) = do
      let msg = if obj.subscribed
                then "This user is subscribed! :)"
                else "This user is not subscribed! :("
      win <- window
      alert msg  win
