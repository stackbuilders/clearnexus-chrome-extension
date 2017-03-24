module Chrome.Storage ( saveToken
                      , addListenerToSaveBtn
                      , PopUpDocument
                      , ChromeObj      ) where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Types (EventType(..))
import DOM.HTML.Types (ALERT)
import DOM.Event.EventTarget (addEventListener, eventListener)
import Data.Either (either)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Null (Null(..), readNull, unNull)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Config (CHROME)
import DOM.QueryDocument (queryDocElt)


type PopUpDocument = { getElementById ::
                          String -> { value :: String }
                     }

type ChromeObj = { storage ::
                      { sync ::
                           { set :: Fn2 Unit Unit Unit }
                      }
                 }


foreign import uncurriedSaveToken :: forall eff .
                                     Fn2 (Null PopUpDocument)
                                         (Null ChromeObj)
                                         (Eff (chrome :: CHROME, alert :: ALERT | eff) Foreign)


saveToken :: forall eff .
             Maybe PopUpDocument
          -> Maybe ChromeObj
          -> Eff ( dom :: DOM
                 , chrome :: CHROME
                 , alert :: ALERT  | eff) (Maybe String)
saveToken optDoc chrome = do
  value <- curried (Null optDoc) (Null chrome)
  let eitherDoc = unNull <$> runExcept (readNull readString value)
  either (const $ pure Nothing) pure eitherDoc
  where
    curried = runFn2 uncurriedSaveToken


addListenerToSaveBtn :: forall eff .
                        Eff ( dom :: DOM
                            , chrome :: CHROME
                            , alert :: ALERT  | eff) Unit
addListenerToSaveBtn = do
  maybeElt <- queryDocElt("[id=save_tkn_cn]")
  case maybeElt of
    Nothing -> pure unit
    Just btn -> do
      addEventListener (EventType "click")
                       (eventListener listener)
                       false
                       btn
  where
    listener _ = do
      saveToken Nothing Nothing
      pure unit
