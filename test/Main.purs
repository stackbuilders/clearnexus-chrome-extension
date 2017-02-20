module Test.Main where

import Data.Either ( Either(..) )
import GenerateClient.Types ( EmailProperties(..)
                            )
import Prelude ( bind
               , ($)
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
notSubscribedEmail = "notsubscribed@clearnex.us"

unsubscribedEmail :: String
unsubscribedEmail = "unsubscribed@clearnex.us"

subscribedEmail :: String
subscribedEmail = "subscribed@clearnex.us"

resubscribedEmail :: String
resubscribedEmail = "resubscribed@clearnex.us"

testUserToken :: String
testUserToken = "chromeExtensionIntegrationTestAccessToken"

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getSubscriptionStatus" do
      testClientNeverSubscribedEmail
      testClientSubscribedEmail
      testClientUnsubscribedEmail
      testClientResubscribedEmail

testClientNeverSubscribedEmail =
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

testClientSubscribedEmail =
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

testClientUnsubscribedEmail =
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

testClientResubscribedEmail =
  it "returns true for an email that has resubscribed" do
    isSubscribed <- getSubscriptionStatus
                      clearNexusStaging
                        resubscribedEmail
                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status ->
        EPInstances status `shouldEqual`
          EPInstances ( EmailProperties { subscribed: true } )
