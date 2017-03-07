module Main where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML.Types (ALERT)
import DOM.QueryDocument (extensionListener, keyPressEvt, queryTextArea)


main :: forall eff . Eff (  alert :: ALERT
                         ,  console :: CONSOLE
                         ,  dom :: DOM | eff  ) Unit
main = do
  maybeTextArea <- queryTextArea
  case maybeTextArea of
    Nothing -> log "Nothing Happened"
    Just textArea -> do
      addEventListener keyPressEvt
                       (eventListener extensionListener)
                       false
                       textArea
