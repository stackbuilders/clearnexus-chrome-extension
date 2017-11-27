module Test.Main where


import Data.Tuple
import Config (CHROME)
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
import Test.PasteLink (testInnerTextForLink, testSelectors)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Storage (testQueryForToken)
import Test.GenClient ( testGetLastMailingWithSubscribedEmail
                      , testGetLastMailingWithUnsubscribedEmail
                      , testGetLastMailingWithResubscribedEmail
                      , testGetLastMailingWithInvalidToken
                      , testGetLastMailingWithNonExistentEmail )


main :: forall eff . Eff (  process :: PROCESS
                          , console :: CONSOLE
                          , timer :: TIMER
                          , avar :: AVAR
                          , ajax :: AJAX
                          , alert :: ALERT
                          , dom :: DOM
                          , chrome :: CHROME | eff  )
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
      describe "Get Last Mailing Data endpoint" do
        describe "getLastMailing" do
          testGetLastMailingWithSubscribedEmail userToken
          testGetLastMailingWithUnsubscribedEmail userToken
          testGetLastMailingWithResubscribedEmail userToken
          testGetLastMailingWithNonExistentEmail userToken
          testGetLastMailingWithInvalidToken
      describe "DOM Module" do
        describe "Querying Gmail's document elements" do
          testQueryForToken
        describe "Pasting link in Gmail's compose box" do
          testInnerTextForLink
          testSelectors
