module Main where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import DOM.QueryDocument (clickEvent, composeBtnListener, delayExtInjection)


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  dom :: DOM | eff  ) Unit
main = delayExtInjection "div[gh=cm]" clickEvent composeBtnListener
