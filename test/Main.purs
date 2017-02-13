module Test.Main where

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff ( Eff )
import Control.Monad.Error.Class ( catchError
                                 , class MonadError )
import Control.Monad.Except.Trans ( runExceptT )
import Control.Monad.Reader.Trans ( runReaderT )
import Control.Monad.Reader.Class ( local
                                  , class MonadReader
                                  )
import Data.Either ( Either(..) )
import Function ( ($)
                , const
                )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail(..)
                            , Token(..)
                            )
import Network.HTTP.Affjax (AJAX)
import Prelude ( bind
               , pure
               , Unit
               , (<$>)
               , (<<<)
               )
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , errorToString
                                 )
import Servant.PureScript.Settings ( SPSettings_(..)
                                   , defaultSettings
                                   )
import ServerAPI ( SPParams_(..)
                 , getApiEmailByEmail
                 )
import Test.Spec ( describe
                 , it
                 )
import Test.Spec.Assertions ( fail
                            , shouldEqual
                            )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run
                        , RunnerEffects
                        )

import Util ( getSubscriptionStatus
            , EPInstances(..)
            )

makeSettings :: { baseURL :: String }
             -> SPSettings_ SPParams_
makeSettings uri = defaultSettings $ SPParams_ uri

clearNexusStaging :: { baseURL :: String }
clearNexusStaging = { baseURL : "https://staging.clearnex.us" }

unsubscribedEmail :: UriEmail
unsubscribedEmail = 
  UriEmail { unUriEmail : "notsubscribed@test.com" } 

testUserToken :: Token
testUserToken = Token { unToken : "testToken" }

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        isSubscribed <- ( runReaderT <<< runExceptT )
                          ( getSubscriptionStatus unsubscribedEmail testUserToken )
                            ( makeSettings clearNexusStaging )
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            EPInstances status `shouldEqual`
              EPInstances ( EmailProperties { subscribed: false } )
