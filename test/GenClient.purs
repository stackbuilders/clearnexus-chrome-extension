module Test.GenClient (  testClientNeverSubscribedEmail
                       , testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail   ) where


import Control.Monad.Aff (Aff)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import GenerateClient.Types (EmailProperties(..))
import Prelude (bind, ($), Unit)
import Servant.PureScript.Affjax (errorToString)
import Network.HTTP.Affjax (AJAX)
import Test.Spec (Group, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Util  (getSubscriptionStatus, EPInstances(..))


type GenClientTest = forall eff .
                       String -> StateT (Array (Group (Aff ( ajax ∷ AJAX | eff ) Unit))) Identity Unit


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


testClientNeverSubscribedEmail :: GenClientTest
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


testClientSubscribedEmail :: GenClientTest
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

testClientUnsubscribedEmail :: GenClientTest
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


testClientResubscribedEmail :: GenClientTest
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
