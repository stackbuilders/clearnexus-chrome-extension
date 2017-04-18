module Main where


import Prelude
import Chrome.Storage (addListenerToSaveBtn)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import DOM.HTML.Types (ALERT)
import DOM.Listener (textAreaListener, CLEANTIMER)
import DOM.QueryDocument (delayExtInjection)
import Network.HTTP.Affjax (AJAX)
import DOM.Event.Types (EventType(..))
import Control.Monad.Aff (runAff)
import Config ( loadConfig
              , setEnv
              , Config(..)
              , CHROME   )


-- << CHANGE THIS VARIABLE ACCORDING TO ENVIRONMENT >> --
environment :: String
environment = "development"
------------------------- << >> -------------------------


main :: forall eff . Eff (  alert :: ALERT
                         ,  timer :: TIMER
                         ,  console :: CONSOLE
                         ,  ajax :: AJAX
                         ,  dom :: DOM
                         , cleantimer :: CLEANTIMER
                         ,  chrome :: CHROME | eff  ) Unit
main = do
  setEnv Nothing environment
  (flip $ runAff (\_ -> log "Chrome Storage Error...")) loadConfig $ \(Config conf) -> do
    liftEff $ addListenerToSaveBtn
    liftEff $ delayExtInjection "div[gh=cm]" (EventType "click") (textAreaListener conf.url "")
  pure unit
