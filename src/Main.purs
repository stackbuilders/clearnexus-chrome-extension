
module Main where


import Prelude
import Chrome.Storage (addListenerToSaveBtn)
import Config (CHROME, environment)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.Event.Types (EventType(..))
import DOM.HTML.Types (ALERT)
import DOM.Listener (textAreaListener)
import DOM.QueryDocument (delayExtInjection)
import Network.HTTP.Affjax (AJAX)
import String ((++))


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  ajax :: AJAX
                         ,  dom :: DOM
                         ,  chrome :: CHROME | eff  ) Unit
main = do
  log $ "Environment: " ++ environment
  addListenerToSaveBtn
  delayExtInjection "div[gh=cm]" (EventType "click") (textAreaListener)
  pure unit
