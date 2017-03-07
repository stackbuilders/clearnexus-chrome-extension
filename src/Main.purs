module Main where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import DOM.QueryDocument (delayExtInjection)
import DOM.Listener (clickEvent, textAreaListener)
import Network.HTTP.Affjax (AJAX)


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  ajax :: AJAX
                         ,  dom :: DOM | eff  ) Unit
main = delayExtInjection "div[gh=cm]" clickEvent textAreaListener
