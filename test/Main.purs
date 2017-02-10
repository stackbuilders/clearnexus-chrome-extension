module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

makeSettings :: String -> SPSettings_ SPParams_
makeSettings uri = SPSettings_ $ SPParams_ uri

clearNexusStaging :: String
clearNexusStaging = "https://staging.clearnex.us"

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        let emailAddr = UriEmail "notsubscribed@test.com"
        let userToken = Token "testToken"
        isSubscribed <- local $ makeSettings clearNexusStaging $
          getApiEmailByEmail emailAddr userToken
        isSubscribed.subscribed `shouldBe` false
