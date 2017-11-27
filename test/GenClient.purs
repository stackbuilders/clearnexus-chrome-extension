module Test.GenClient (  testGetLastMailingWithSubscribedEmail
                       , testGetLastMailingWithUnsubscribedEmail
                       , testGetLastMailingWithResubscribedEmail
                       , testGetLastMailingWithInvalidToken
                       , testGetLastMailingWithNonExistentEmail ) where


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


testGetLastMailingWithNonExistentEmail :: forall eff . String -> GenClientTest eff
testGetLastMailingWithNonExistentEmail userToken =
  it "returns Status Code 404 when called with an email which is not in the Server's DB" do
    response <- getLastMailing clearNexusStaging "nonexistentemail@clearnex.us" userToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 404)"


testGetLastMailingWithSubscribedEmail :: forall eff . String -> GenClientTest eff
testGetLastMailingWithSubscribedEmail testUserToken =
  it "returns MailingData related to a subscribed link" do
    mailing <- getLastMailing clearNexusStaging subscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` true
      _ -> fail $ "Unknow error"    


testGetLastMailingWithUnsubscribedEmail :: forall eff . String -> GenClientTest eff
testGetLastMailingWithUnsubscribedEmail testUserToken =
  it "returns MailingData related to a unsubscribed link" do
    mailing <- getLastMailing clearNexusStaging notSubscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err      
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` false
      _ -> fail $ "Unknow error"
      

testGetLastMailingWithResubscribedEmail :: forall eff . String -> GenClientTest eff
testGetLastMailingWithResubscribedEmail testUserToken =
  it "returns MailingData related to a subscribed link" do
    mailing <- getLastMailing clearNexusStaging resubscribedEmail testUserToken
    case mailing of
      Left err -> fail $ errorToString err
      Right (LastMailingData { mailing_data: ml }) -> getSubsStatus ml `shouldEqual` true
      _ -> fail $ "Unknow error"


testGetLastMailingWithInvalidToken :: forall eff . GenClientTest eff
testGetLastMailingWithInvalidToken =
  it "returns Status Code 401 when called with Invalid Token" do
    response <- getLastMailing clearNexusStaging resubscribedEmail invalidToken
    getStatusCodeFromErrDesc response `shouldEqual` "(StatusCode 401)"

-- << TODO: We need a way to add rollbacks on the server side for testing
--    in order for this test to be replicable because it always creates a new
--    link in DB. After that we could test post client endpoints.
