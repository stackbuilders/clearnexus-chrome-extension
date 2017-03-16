module Test.Main where


import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Node.Process (PROCESS, lookupEnv, exit)
import Prelude (Unit, bind)
import Test.QueryEmail (testQueryForDivTags, testEmailExtraction)
import Test.Spec (describe, pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Storage (testQueryForToken)
import Test.GenClient ( testApiCallWithInvalidToken
                      , testApiCallWithNonExistentEmail
                      , testClientResubscribedEmail
                      , testClientSubscribedEmail
                      , testClientUnsubscribedEmail
                      , testPostNewLinkWithUnsuscribedEmail )


main :: forall eff . Eff (  process :: PROCESS
                          , console :: CONSOLE
                          , timer :: TIMER
                          , avar :: AVAR
                          , ajax :: AJAX
                          , alert :: ALERT
                          , dom :: DOM | eff )
                          Unit
main =  do
  maybeToken <- lookupEnv "CLEARNEXUS_CLIENT_TEST_TOKEN"
  case maybeToken of
    Nothing -> do
      log "Please set the CLEARNEXUS_CLIENT_TEST_TOKEN environment variable..."
      exit 1
    Just token -> run [ consoleReporter ] do
      describe "Generated Client" do
        describe "getSubscriptionStatus" do
          testClientSubscribedEmail token
          testClientUnsubscribedEmail token
          testClientResubscribedEmail token
          testApiCallWithInvalidToken
          testApiCallWithNonExistentEmail token
      describe "DOM Module" do
        describe "Querying Gmail's document elements" do
          testQueryForDivTags
          testEmailExtraction
          testQueryForToken
      describe "Post New LinkData" do
        pending "postNewlink"
