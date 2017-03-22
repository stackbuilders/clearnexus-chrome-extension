module Test.GenClient (  testGetEmailPropsWithSubscribedEmail
                       , testGetEmailPropsWithUnsubscribedEmail
                       , testGetEmailPropsWithResubscribedEmail
                       , testGetEmailPropsWithInvalidToken
                       , testGetEmailPropsWithNonExistentEmail
                       , testGetLinkWithInvalidUserToken
                       , testPostNewLinkWithUnsuscribedEmail
                       , testGetLinkWithInvalidLinkToken
                       , testGetLinkWithValidTokens          ) where


import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Semigroup ((<>))
import GenerateClient.Types (EmailProperties(..), LinkData(..))
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, ($), Unit, show)
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , ErrorDescription(..)
                                 , errorToString      )
import Test.Spec (Group, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)
import Util (getSubscriptionStatus, postNewLink, getLink)


type GenClientTest eff = forall eff . StateT (Array (Group (Aff ( console :: CONSOLE, ajax ∷ AJAX | eff ) Unit))) Identity Unit


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

invalidToken :: String
invalidToken = "INVALID-TOKEN"


-- << Helpers
getStatusCodeFromErrDesc :: forall a . Either AjaxError a -> String
getStatusCodeFromErrDesc (Right _) = "There was no Error"
getStatusCodeFromErrDesc (Left (AjaxError err)) = errorDescToString err.description
  where
    errorDescToString :: ErrorDescription -> String
    errorDescToString (ConnectionError desc) = desc
    errorDescToString (DecodingError desc) = desc
    errorDescToString (ParsingError desc) = desc
    errorDescToString (UnexpectedHTTPStatus obj) = show $ obj.status

getStatus :: EmailProperties -> Boolean
getStatus (EmailProperties obj) = obj.subscribed
-- >>


testGetEmailPropsWithNonExistentEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithNonExistentEmail userToken =
  it "returns Status Code 404 when called with an email which is not in the Server's DB" do
    response <- getSubscriptionStatus clearNexusStaging
                                      notSubscribedEmail
                                      userToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 404)"


testGetEmailPropsWithSubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithSubscribedEmail testUserToken =
  it "returns true for an email that is subscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          subscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual` true


testGetEmailPropsWithUnsubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithUnsubscribedEmail testUserToken =
  it "returns false for an email that has unsubscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          unsubscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual` false


testGetEmailPropsWithResubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithResubscribedEmail testUserToken =
  it "returns true for an email that has resubscribed" do
    isSubscribed <- getSubscriptionStatus clearNexusStaging
                                          resubscribedEmail
                                          testUserToken
    case isSubscribed of
      Left err -> fail $ errorToString err
      Right status -> getStatus status `shouldEqual`  true


testGetEmailPropsWithInvalidToken :: forall eff . GenClientTest eff
testGetEmailPropsWithInvalidToken =
  it "returns Status Code 500 when called with Invalid Token" do
    response <- getSubscriptionStatus clearNexusStaging resubscribedEmail invalidToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 500)"


testGetLinkWithInvalidUserToken :: forall eff . String -> GenClientTest eff
testGetLinkWithInvalidUserToken linkToken =
  it "returns Status Code 500 when called with Invalid User Token" do
    response <- getLink clearNexusStaging linkToken invalidToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 500)"


testGetLinkWithInvalidLinkToken :: forall eff . String -> GenClientTest eff
testGetLinkWithInvalidLinkToken userToken =
  it "returns Status Code 500 when called with Invalid User Token" do
    response <- getLink clearNexusStaging invalidToken userToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 500)"


testGetLinkWithValidTokens :: forall eff . String -> String -> GenClientTest eff
testGetLinkWithValidTokens linkToken userToken =
  it "returns a LinkData object with the correct link properties" do
    response <- getLink clearNexusStaging linkToken userToken
    case response  of
      (Right (LinkData linkData)) -> do
        linkData.email `shouldContain` "@clearnex.us"
        linkData.organization `shouldEqual` "Chrome Extension Test"
        linkData.token `shouldEqual` linkToken
        linkData.unsubscription_link `shouldContain` ("links/" <> linkToken <> "/unsubscribe")
        linkData.subscription_link `shouldContain` ("links/" <> linkToken <> "/subscribe")
      err -> logShow $ getStatusCodeFromErrDesc err


-- << TODO: We need a way to add rollbacks on the server side for testing
--    in order for this test to be replicable because it always creates a new
--    link in DB. After that, match this test with the appropriate values.
testPostNewLinkWithUnsuscribedEmail :: forall eff . String -> GenClientTest eff
testPostNewLinkWithUnsuscribedEmail userToken =
  it "returns a LinkData object with the correct link properties" do
    response <- postNewLink clearNexusStaging
                            "exampleEmail@gmail.com"
                            userToken
    case response  of
      (Right (LinkData linkData)) -> do
        linkData.email `shouldEqual` ""
        linkData.organization `shouldEqual` ""
        linkData.token `shouldEqual` ""
        linkData.unsubscription_link `shouldEqual` ""
        linkData.subscription_link `shouldEqual` ""
        linkData.created_at `shouldEqual` ""
      err -> logShow $ getStatusCodeFromErrDesc err
