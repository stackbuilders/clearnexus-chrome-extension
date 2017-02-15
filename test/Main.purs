module Test.Main where

import Data.Either ( Either(..) )
import Function ( ($) )
import GenerateClient.Types ( EmailProperties(..)
                            )
import Prelude ( bind
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
clearNexusStaging = { baseURL : "https://staging.clearnex.us/" }

notSubscribedEmail :: String
notSubscribedEmail = "notsubscribed@testing.com"

unsubscribedEmail :: String
unsubscribedEmail = "unsubscribed@testing.com"

subscribedEmail :: String
subscribedEmail = "subscribed@testing.com"

testUserToken :: String
testUserToken = "18bfa273-0107-47c4-9aff-f7cd487bc19b"

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that has never subscribed" do
        isSubscribed <- getSubscriptionStatus
                          clearNexusStaging
                            notSubscribedEmail
                              testUserToken
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            EPInstances status `shouldEqual`
              EPInstances ( EmailProperties { subscribed: false } )
      it "returns true for an email that is subscribed" do
        isSubscribed <- getSubscriptionStatus
                          clearNexusStaging
                            subscribedEmail
                              testUserToken
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            EPInstances status `shouldEqual`
              EPInstances ( EmailProperties { subscribed: true } )
      it "returns false for an email that has unsubscribed" do
        isSubscribed <- getSubscriptionStatus
                          clearNexusStaging
                            unsubscribedEmail
                              testUserToken
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            EPInstances status `shouldEqual`
              EPInstances ( EmailProperties { subscribed: false } )
