module Test.PasteLink where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State.Trans (StateT)
import DOM (DOM)
import DOM.QueryDocument (DocumentElement, pasteLink)
import Data.Array (length)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Prelude (Unit, (==), (||), bind, ($), unit)
import Test.Spec (it, Group)
import Test.Spec.Assertions (shouldEqual)


type PasteLinkTest = forall eff .
                     StateT (Array (Group (Aff (dom ∷ DOM | eff) Unit))) Identity Unit


composeSelector :: String
composeSelector = "div.Am.Al.editable.LW-avf"

signatureSelector :: String
signatureSelector = "div[data-smartmail=gmail_signature]"


-- <<  Mock to test the correct searching of <div> tags with class vR
linkText :: DocumentElement
linkText = {  getElementsByClassName: \_ -> []
           ,  querySelector: \_ -> Just {
                 firstChild: unit
               , appendChild: (\_ -> unit)
               , insertBefore: (\_ -> unit)
               }
           , createElement: \_ -> {
                 innerText: ""
               , href: ""
               }
           }

-- << Mock to test the correct selector queries passed to this function
selectors :: DocumentElement
selectors = {  getElementsByClassName: \_ -> []
            ,  querySelector: \selector ->
                if selector == composeSelector || selector == signatureSelector
                then Just {
                 firstChild: unit
               , appendChild: (\_ -> unit)
               , insertBefore: (\_ -> unit)
               }
                else Nothing
           , createElement: \_ -> {
                 innerText: ""
               , href: ""
               }
           }


testInnerTextForLink :: PasteLinkTest
testInnerTextForLink =
  it "returns the correct inner text for the <a> tag" do
    value <- liftEff $ pasteLink (Just linkText) "<SOME LINK>"
    value `shouldEqual` Just "Click here to unsubscribe"


testSelectors :: PasteLinkTest
testSelectors =
  it "requeries the correct selectors in the Gmail's document" do
    value <- liftEff $ pasteLink (Just selectors) "<SOME LINK>"
    value `shouldEqual` Just "Click here to unsubscribe"
