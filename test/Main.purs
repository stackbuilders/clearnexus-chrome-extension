module Test.Main where

import Control.Monad.Reader.Class ( local )
import Function ( ($)
                , const
                )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail(..)
                            , Token(..)
                            )
import Prelude ( bind )
import Servant.PureScript.Settings ( SPSettings_(..)
                                   , defaultSettings
                                   )
import ServerAPI ( SPParams_(..)
                 , getApiEmailByEmail
                 )
import Test.Spec ( describe
                 , it
                 )
import Test.Spec.Assertions ( shouldEqual )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run )

makeSettings :: { baseURL :: String }
             -> SPSettings_ SPParams_
             -> SPSettings_ SPParams_
makeSettings uri = const $ defaultSettings $ SPParams_ uri

clearNexusStaging :: { baseURL :: String }
clearNexusStaging = { baseURL : "https://staging.clearnex.us" }

main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        let emailAddr = UriEmail
              { unUriEmail : "notsubscribed@test.com" }
            userToken = Token
              { unToken : "testToken" }
        isSubscribed <- local ( makeSettings clearNexusStaging ) $
          getApiEmailByEmail emailAddr userToken
        isSubscribed `shouldEqual` EmailProperties
                                     { subscribed: false }
