module Test.GenClient (  testGetEmailPropsWithSubscribedEmail
                       , testGetEmailPropsWithUnsubscribedEmail
                       , testGetEmailPropsWithResubscribedEmail
                       , testGetEmailPropsWithInvalidToken
                       , testGetEmailPropsWithNonExistentEmail
                       , testGetLinkWithInvalidUserToken
                       , testGetLinkWithInvalidLinkToken
                       , testGetLinkWithValidTokens          ) where


import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..))
import GenerateClient.Types 
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, ($), Unit, show)
import Config (CHROME)
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , ErrorDescription(..)
                                 , errorToString      )
import Test.Spec (Group, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)
import Util (getLastMailing)

type GenClientTest eff =
  forall eff .
  StateT (Array (Group (Aff ( chrome :: CHROME, console :: CONSOLE, ajax ∷ AJAX | eff ) Unit))) Identity Unit


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

getSubsStatus :: Maybe MailingData -> Boolean
getSubsStatus Nothing = true
getSubsStatus (Just (MailingData ml)) = ml.is_link_subscribed
-- >>


testGetEmailPropsWithNonExistentEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithNonExistentEmail userToken =
  it "returns Status Code 404 when called with an email which is not in the Server's DB" do
    response <- getLastMailing clearNexusStaging notSubscribedEmail userToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 404)"


testGetEmailPropsWithSubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithSubscribedEmail testUserToken =
  it "returns MailingData related to a subscribed link" do
    mailing <- getLastMailing clearNexusStaging subscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` true
      _ -> fail $ "Unknow error"    


testGetEmailPropsWithUnsubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithUnsubscribedEmail testUserToken =
  it "returns MailingData related to a subscribed link" do
    mailing <- getLastMailing clearNexusStaging unsubscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err      
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` false
      _ -> fail $ "Unknow error"
      

testGetEmailPropsWithResubscribedEmail :: forall eff . String -> GenClientTest eff
testGetEmailPropsWithResubscribedEmail testUserToken =
  it "returns MailingData related to a subscribed link" do
    mailing <- getLastMailing clearNexusStaging resubscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` true
      _ -> fail $ "Unknow error"


testGetEmailPropsWithInvalidToken :: forall eff . GenClientTest eff
testGetEmailPropsWithInvalidToken =
  it "returns Status Code 401 when called with Invalid Token" do
    response <- getLastMailing clearNexusStaging resubscribedEmail invalidToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 401)"


testGetLinkWithInvalidUserToken :: forall eff . String -> GenClientTest eff
testGetLinkWithInvalidUserToken linkToken =
  it "returns Status Code 401 when called with Invalid User Token" do
    response <- getLastMailing clearNexusStaging linkToken invalidToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 401)"


testGetLinkWithInvalidLinkToken :: forall eff . String -> GenClientTest eff
testGetLinkWithInvalidLinkToken userToken =
  it "returns Status Code 500 when called with Invalid Link Token" do
    response <- getLastMailing clearNexusStaging invalidToken userToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 500)"


testGetLinkWithValidTokens :: forall eff . String -> String -> GenClientTest eff
testGetLinkWithValidTokens linkToken userToken =
  it "returns a MailingData object with the correct link properties" do
    response <- getLastMailing clearNexusStaging linkToken userToken
    case response  of
      Right (LastMailingData { mailing_data: Just (MailingData ml) }) -> do
        ml.email `shouldContain` "@clearnex.us"
        ml.organization `shouldEqual` "Chrome Extension Test"
        ml.token `shouldEqual` linkToken
        ml.unsubscription_link `shouldContain` ("links/" <> linkToken <> "/unsubscribe")
        ml.subscription_link `shouldContain` ("links/" <> linkToken <> "/subscribe")
      err -> logShow $ getStatusCodeFromErrDesc err


-- << TODO: We need a way to add rollbacks on the server side for testing
--    in order for this test to be replicable because it always creates a new
--    link in DB. After that we could test post client endpoints.
