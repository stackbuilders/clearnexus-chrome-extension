module DOM.QueryEmail ( readEmails
                      , readEmails_
                      , DomMock     ) where

import Prelude
import Control.Monad.Eff ( Eff )
import Control.Monad.Except ( runExcept )
import DOM ( DOM )
import Data.Either ( either )
import Data.Foreign ( Foreign, readArray, readString )
import Data.Traversable ( traverse )


type DomMock = { getElementsByTagName :: String
                                      -> Array { value :: String
                                               , getAttribute :: String -> String }  }


foreign import queryEmails :: forall eff . Eff (dom :: DOM | eff) Foreign

--It receives a mock and returns a Foreign entity. Only for Testing
foreign import queryEmails_ :: forall eff . DomMock -> Eff (dom :: DOM | eff) Foreign


-- It uses queryEmails_ so that we can provide a mock for Testing
readEmails_ :: forall eff . DomMock
                         -> Eff (dom :: DOM | eff) (Array String)
readEmails_ mock = do
  query <- queryEmails_ mock -- provide the mock
  values <- pure $ either (\_ -> []) id (runExcept $ readArray query)
  emails <- pure $ either (\_ -> []) id (runExcept $ traverse readString values)
  pure emails

-- This is the function which is used in the Browser
readEmails :: forall eff . Eff (dom :: DOM | eff) (Array String)
readEmails = do
  query <- queryEmails
  values <- pure $ either (\_ -> []) id (runExcept $ readArray query)
  emails <- pure $ either (\_ -> []) id (runExcept $ traverse readString values)
  pure emails
