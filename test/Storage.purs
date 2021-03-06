module Test.Storage (testQueryForToken) where


import Prelude (Unit, bind, ($), (==), unit)
import Chrome.Storage (PopUpDocument, ChromeObj, saveToken)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State.Trans (StateT)
import DOM (DOM)
import DOM.HTML.Types (ALERT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Test.Spec (it, Group)
import Test.Spec.Assertions (shouldEqual)
import Config (CHROME)
import Data.Function.Uncurried (mkFn2)


type QueryTokenTest =
  forall eff .
  StateT (Array (Group (Aff (chrome :: CHROME, dom ∷ DOM, alert :: ALERT | eff) Unit))) Identity Unit


-- << Mock to test the correct searching in options document by "authtoken" id in <input> tag
doc :: PopUpDocument
doc = { getElementById: (\str ->  if str == "authtoken_cn"
                                    then { value: "Some-Token" }
                                    else { value: "Invalid-Id" })
      }


-- << Mock of the chrome object provided for extensions
chrome :: ChromeObj
chrome = { storage: {
              sync: {
                 set: mkFn2 (\ _ _ -> unit)
                 }
              }
         }


testQueryForToken :: QueryTokenTest
testQueryForToken =
  it "passes *authoken_cn* id to the JS function which queries the clearnexus token" do
    maybeToken <- liftEff $ saveToken (Just doc) (Just chrome)
    maybeToken `shouldEqual` Just "Some-Token"
