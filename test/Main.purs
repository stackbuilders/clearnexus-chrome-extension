module Test.Main where

import Control.Monad.Eff.Console ( log )
import Data.Maybe ( Maybe(..) )
import Prelude ( bind )
import Test.Spec ( describe )
import Node.Process ( lookupEnv )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run )
import Test.QueryEmail ( dummyTest )
import Test.GenClient (  testClientNeverSubscribedEmail
                       , testClientSubscribedEmail
                       , testClientUnsubscribedEmail
                       , testClientResubscribedEmail  )


main =  do
  maybeToken <- lookupEnv "CLEARNEXUS_CLIENT_TEST_TOKEN"
  case maybeToken of
    Nothing ->
      log "Please set the CLEARNEXUS_CLIENT_TEST_TOKEN environment var│expecting identifier, string literal or variable."
    Just token -> run [ consoleReporter ] do
      describe "Generated Client" do
        describe "getSubscriptionStatus" do
          testClientNeverSubscribedEmail token
          testClientSubscribedEmail token
          testClientUnsubscribedEmail token
          testClientResubscribedEmail token
          dummyTest
