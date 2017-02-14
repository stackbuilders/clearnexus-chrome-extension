module Test.Main where

import Data.Either ( Either(..) )
import Function ( ($) )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail(..)
                            , Token(..)
                            )
import Prelude ( bind
               , (<<<)
               )
import Servant.PureScript.Affjax ( errorToString )
import Test.Spec ( describe
                 , it
                 )
import Test.Spec.Assertions ( fail
                            , shouldEqual
                            )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run )

import Util ( getSubscriptionStatus
            , EPInstances(..)
            )

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
        isSubscribed <- getSubscriptionStatus
                          clearNexusStaging
                            unsubscribedEmail
                              testUserToken
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            EPInstances status `shouldEqual`
              EPInstances ( EmailProperties { subscribed: false } )
