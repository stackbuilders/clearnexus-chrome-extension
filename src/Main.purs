module Main where


import Prelude
import Chrome.Storage (addListenerToSaveBtn)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import DOM.Listener (clickEvent, textAreaListener)
import DOM.QueryDocument (delayExtInjection)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  ajax :: AJAX
                         ,  dom :: DOM | eff  ) Unit
main = do
 addListenerToSaveBtn
  --delayExtInjection "div[gh=cm]" clickEvent textAreaListener
