module Test.GenClient (  testClientNeverSubscribedEmail
                       , testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail   ) where

import Data.Either ( Either(..) )
import GenerateClient.Types ( EmailProperties(..) )
import Prelude ( bind, ($) )
import Servant.PureScript.Affjax ( errorToString )
import Test.Spec ( it )
import Test.Spec.Assertions ( fail, shouldEqual )
import Util ( getSubscriptionStatus, EPInstances(..) )


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


testClientNeverSubscribedEmail testUserToken =
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

testClientSubscribedEmail testUserToken =
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

testClientUnsubscribedEmail testUserToken =
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

testClientResubscribedEmail testUserToken =
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
