module Test.Main where


import Control.Monad.Eff.Console ( CONSOLE, log )
import Control.Monad.Eff ( Eff )
import Control.Monad.Aff.AVar ( AVAR )
import Network.HTTP.Affjax ( AJAX )
import Control.Monad.Eff.Timer ( TIMER )
import Data.Maybe ( Maybe(..) )
import Prelude ( Unit, bind )
import DOM ( DOM )
import Test.Spec ( describe )
import Node.Process ( PROCESS, lookupEnv, exit )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run )
import Test.QueryEmail (  testFilteringOfInputElements
                        , testQueryForInputTag
                        , testQueryForNameAttr        )
import Test.GenClient (  testClientNeverSubscribedEmail
                       , testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail  )


main :: forall eff . Eff (  process :: PROCESS
                          , console :: CONSOLE
                          , timer :: TIMER
                          , avar :: AVAR
                          , ajax :: AJAX
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
          testClientNeverSubscribedEmail token
          testClientSubscribedEmail token
          testClientUnsubscribedEmail token
          testClientResubscribedEmail token
          testFilteringOfInputElements
          testQueryForInputTag
          testQueryForNameAttr
