module Test.GenClient (  testClientNeverSubscribedEmail
                       , testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail
                       , testApiCallWithInvalidToken   ) where


import Prelude (bind, ($), Unit, show)
import Control.Monad.Aff (Aff)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import GenerateClient.Types (EmailProperties(..))
import Network.HTTP.Affjax (AJAX)
import Test.Spec (Group, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Util  (getSubscriptionStatus)
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , ErrorDescription(..)
                                 , errorToString      )


type GenClientTest = forall eff .
                     String -> StateT (Array (Group (Aff ( ajax ∷ AJAX | eff ) Unit))) Identity Unit


clearNexusStaging :: String
clearNexusStaging = "https://staging.clearnex.us/"

notSubscribedEmail :: String
notSubscribedEmail = "notsubscribed@clearnex.us"

unsubscribedEmail :: String
unsubscribedEmail = "unsubscribed@clearnex.us"

subscribedEmail :: String
subscribedEmail = "subscribed@clearnex.us"

resubscribedEmail :: String
resubscribedEmail = "resubscribed@clearnex.us"


-- << Helper Functions
resToString :: Either AjaxError EmailProperties -> String
resToString (Right _) = "No Error"
resToString (Left (AjaxError err)) = errorDescToString err.description
  where
    errorDescToString :: ErrorDescription -> String
    errorDescToString (ConnectionError desc) = desc
    errorDescToString (DecodingError desc) = desc
    errorDescToString (ParsingError desc) = desc
    errorDescToString (UnexpectedHTTPStatus obj) = show $ obj.status

getStatus :: EmailProperties -> Boolean
getStatus (EmailProperties obj) = obj.subscribed
-- >>


testClientNeverSubscribedEmail :: GenClientTest
testClientNeverSubscribedEmail testUserToken =
  it "returns false for an email that has never subscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          notSubscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual` false


testClientSubscribedEmail :: GenClientTest
testClientSubscribedEmail testUserToken =
  it "returns true for an email that is subscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          subscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual` true


testClientUnsubscribedEmail :: GenClientTest
testClientUnsubscribedEmail testUserToken =
  it "returns false for an email that has unsubscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          unsubscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual` false


testClientResubscribedEmail :: GenClientTest
testClientResubscribedEmail testUserToken =
  it "returns true for an email that has resubscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          resubscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual`  true


-- << Test a call to the API with an invalid authetication token
testApiCallWithInvalidToken :: forall eff .
                          StateT (Array (Group (Aff ( ajax ∷ AJAX | eff ) Unit))) Identity Unit
testApiCallWithInvalidToken =
  it "returns Status Code 500 when called with Invalid Token" do
    response <- getSubscriptionStatus clearNexusStaging
                                          resubscribedEmail
                                          "SOME-INVALID-TOKEN"
    resToString response `shouldEqual` "(StatusCode 500)"
