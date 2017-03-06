module Main where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML.Types (ALERT)


main :: forall eff . Eff (  alert :: ALERT
                         ,  console :: CONSOLE
                         ,  dom :: DOM | eff  ) Unit
main = do
  pure unit
