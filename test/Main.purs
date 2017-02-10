module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        let emailAddr = UriEmail "notsubscribed@test.com"
        let userToken = Token "testToken"
        isSubscribed <- local $ const "stagingURI" $
          getApiEmailByEmail emailAddr userToken
        isSubscribed.subscribed `shouldBe` false
