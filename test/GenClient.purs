module Test.GenClient (  testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail
                       , testApiCallWithInvalidToken
                       , testApiCallWithNonExistentEmail
                       , testPostNewLinkWithUnsuscribedEmail ) where


import Prelude (bind, ($), Unit, show)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Aff (Aff)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import GenerateClient.Types (EmailProperties(..), LinkData(..))
import Network.HTTP.Affjax (AJAX)
import Test.Spec (Group, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Util  (getSubscriptionStatus, postNewLink)
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , ErrorDescription(..)
                                 , errorToString      )


type GenClientTest = forall eff .
                     String -> StateT (Array (Group (Aff ( console :: CONSOLE, ajax ∷ AJAX | eff ) Unit))) Identity Unit


clearNexusStaging :: String
clearNexusStaging = "http://localhost:8000/"

notSubscribedEmail :: String
notSubscribedEmail = "notsubscribed@clearnex.us"

unsubscribedEmail :: String
unsubscribedEmail = "unsubscribed@clearnex.us"

subscribedEmail :: String
subscribedEmail = "subscribed@clearnex.us"

resubscribedEmail :: String
resubscribedEmail = "resubscribed@clearnex.us"


-- << Helper Functions
resToString :: forall a . Either AjaxError a -> String
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


-- << Test a call to the API with an email which does not exists
testApiCallWithNonExistentEmail :: GenClientTest
testApiCallWithNonExistentEmail userToken =
  it "returns Status Code 404 when called with an email which is not in the Server's DB" do
    response <- getSubscriptionStatus clearNexusStaging
                                      notSubscribedEmail
                                      userToken
    resToString response `shouldEqual` "(StatusCode 404)"


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


-- << TODO: We need a way to add rollbacks on the server side for testing
--    in order for this test to be replicable because it always creates a new
--    link in DB.
testPostNewLinkWithUnsuscribedEmail :: GenClientTest
testPostNewLinkWithUnsuscribedEmail userToken =
  it "returns a LinkData object with the correct link properties" do
    response <- postNewLink clearNexusStaging
                            "spulido@gmail.com"
                            userToken
    case response  of
      (Right (LinkData linkData)) -> do
        linkData.email `shouldEqual` ""
        linkData.organization `shouldEqual` ""
        linkData.token `shouldEqual` ""
        linkData.unsubscription_link `shouldEqual` ""
        linkData.subscription_link `shouldEqual` ""
        linkData.created_at `shouldEqual` ""
      err -> logShow $ resToString err
