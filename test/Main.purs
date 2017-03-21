module Test.Main where


import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import Data.Maybe (Maybe(..))
import Data.Tuple
import Network.HTTP.Affjax (AJAX)
import Node.Process (PROCESS, lookupEnv, exit)
import Prelude (Unit, bind)
import Test.QueryEmail (testQueryForDivTags, testEmailExtraction)
import Test.Spec (describe, pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Storage (testQueryForToken)
import Test.GenClient (  testGetEmailPropsWithSubscribedEmail
                       , testGetEmailPropsWithUnsubscribedEmail
                       , testGetEmailPropsWithResubscribedEmail
                       , testGetEmailPropsWithInvalidToken
                       , testGetEmailPropsWithNonExistentEmail
                       , testPostNewLinkWithUnsuscribedEmail
                       , testGetLinkWithInvalidUserToken
                       , testGetLinkWithInvalidLinkToken
                       , testGetLinkWithValidTokens          )


main :: forall eff . Eff (  process :: PROCESS
                          , console :: CONSOLE
                          , timer :: TIMER
                          , avar :: AVAR
                          , ajax :: AJAX
                          , alert :: ALERT
                          , dom :: DOM | eff )
                          Unit
main =  do
  maybeUserToken <- lookupEnv "CLEARNEXUS_USER_TEST_TOKEN"
  maybeLinkToken <- lookupEnv "CLEARNEXUS_LINK_TEST_TOKEN"
  case Tuple maybeUserToken maybeLinkToken of
    Tuple Nothing _-> do
      log "Please set the CLEARNEXUS_USER_TEST_TOKEN environment variable..."
      exit 1
    Tuple _ Nothing -> do
      log "Please set the CLEARNEXUS_LINK_TEST_TOKEN environment variable..."
      exit 1
    Tuple (Just userToken) (Just linkToken) -> run [ consoleReporter ] do
      describe "Get email properties endpoint" do
        describe "getSubscriptionStatus" do
          testGetEmailPropsWithSubscribedEmail userToken
          testGetEmailPropsWithUnsubscribedEmail userToken
          testGetEmailPropsWithResubscribedEmail userToken
          testGetEmailPropsWithNonExistentEmail userToken
          testGetEmailPropsWithInvalidToken
      describe "Get link data endpoint" do
        describe "" do
          testGetLinkWithInvalidUserToken linkToken
          testGetLinkWithInvalidLinkToken userToken
          testGetLinkWithValidTokens linkToken userToken
      describe "Post new link endpoint" do
        pending "postNewlink"
      describe "DOM Module" do
        describe "Querying Gmail's document elements" do
          testQueryForDivTags
          testEmailExtraction
          testQueryForToken
