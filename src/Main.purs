module Main where


import Prelude
import Chrome.Storage (addListenerToSaveBtn)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import DOM.Listener (textAreaListener)
import DOM.QueryDocument (delayExtInjection)
import Network.HTTP.Affjax (AJAX)
import DOM.Event.Types (EventType(..))
import Control.Monad.Aff (runAff)
import Config


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  ajax :: AJAX
                         ,  dom :: DOM | eff  ) Unit
main = do
  canceller <- runAff (\error -> pure unit) (\res -> pure unit) $ do
    config <- loadConfig
    liftEff $ addListenerToSaveBtn
    liftEff $ delayExtInjection "div[gh=cm]" (EventType "click") (textAreaListener config)
  pure unit
