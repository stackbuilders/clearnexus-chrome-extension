module Test.Main where

import Control.Monad.Eff ( Eff )
import Control.Monad.Error.Class ( catchError )
import Control.Monad.Reader.Class ( local )
import Function ( ($)
                , const
                )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail(..)
                            , Token(..)
                            )
import Prelude ( bind
               , pure
               , Unit
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

makeSettings :: { baseURL :: String }
             -> SPSettings_ SPParams_
             -> SPSettings_ SPParams_
makeSettings uri = const $ defaultSettings $ SPParams_ uri

clearNexusStaging :: { baseURL :: String }
clearNexusStaging = { baseURL : "https://staging.clearnex.us" }

unsubscribedEmail :: UriEmail
unsubscribedEmail = 
  UriEmail { unUriEmail : "notsubscribed@test.com" } 

testUserToken :: Token
testUserToken = Token { unToken : "testToken" }

main :: Eff ( RunnerEffects () ) Unit
main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        isSubscribed <- catchError
          ( local
            ( makeSettings clearNexusStaging )
            $ getApiEmailByEmail unsubscribedEmail testUserToken )
          $ \e -> pure ( EmailProperties { subscribed : true } )
        isSubscribed `shouldEqual`
                    EmailProperties { subscribed: false }
