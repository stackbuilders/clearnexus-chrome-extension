module Test.Main where

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff ( Eff )
import Control.Monad.Error.Class ( catchError
                                 , class MonadError )
import Control.Monad.Reader.Class ( local
                                  , class MonadReader
                                  )
import Data.Either ( Either(..) )
import Function ( ($)
                , const
                )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail(..)
                            , Token(..)
                            )
import Network.HTTP.Affjax (AJAX)
import Prelude ( bind
               , (<$>)
               , pure
               , Unit
               )
import Servant.PureScript.Affjax ( AjaxError(..)
                                 , errorToString
                                 )
import Servant.PureScript.Settings ( SPSettings_(..)
                                   , defaultSettings
                                   )
import ServerAPI ( SPParams_(..)
                 , getApiEmailByEmail
                 )
import Test.Spec ( describe
                 , it
                 )
import Test.Spec.Assertions ( fail
                            , shouldEqual
                            )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run
                        , RunnerEffects
                        )

makeSettings :: { baseURL :: String }
             -> SPSettings_ SPParams_
             -> SPSettings_ SPParams_
makeSettings uri = const $ defaultSettings $ SPParams_ uri

clearNexusStaging :: { baseURL :: String }
clearNexusStaging = { baseURL : "https://staging.clearnex.us" }

unsubscribedEmail :: UriEmail
unsubscribedEmail = 
  UriEmail { unUriEmail : "notsubscribed@test.com" } 

testUserToken :: Token
testUserToken = Token { unToken : "testToken" }

newtype ReadEither a = ReadEither ( Either AjaxError a )

subscriptionStatus :: forall eff m.
                      (MonadReader (SPSettings_ SPParams_) m, MonadError AjaxError m, MonadAff ( ajax :: AJAX | eff) m) => UriEmail
                   -> m ( Either AjaxError EmailProperties )
subscriptionStatus email = catchError
  ( Right <$>
    ( local
      ( makeSettings clearNexusStaging )
      $ getApiEmailByEmail email testUserToken ) )
  ( \err -> pure $ Left err )

main :: Eff ( RunnerEffects () ) Unit
main = run [ consoleReporter ] do
  describe "Generated Client" do
    describe "getApiEmailByEmail" do
      it "returns false for an email that is not subscribed" do
        isSubscribed <- subscriptionStatus unsubscribedEmail
        case isSubscribed of
          Left err -> fail $ errorToString err
          Right status ->
            status `shouldEqual`
              EmailProperties { subscribed: false }
