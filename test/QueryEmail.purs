module Test.QueryEmail ( testFilteringOfInputElements
                       , testQueryForInputTag
                       , testQueryForNameAttr        ) where


import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff ( Aff )
import DOM.QueryEmail ( readEmails_, HtmlElt )
import DOM ( DOM )
import Data.Array ( length )
import Data.Identity ( Identity )
import Prelude ( bind, ($), Unit )
import Test.Spec ( it, Group )
import Control.Monad.State.Trans ( StateT )
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)


type QueryEmailTest = forall eff .
                        StateT ( Array ( Group ( Aff ( dom ∷ DOM | eff ) Unit ) ) ) Identity Unit


--- <<  Mock to test the correct searching of <input> tags
inputTags :: HtmlElt
inputTags = { getElementsByTagName: \str ->
               case str of
                 "input" -> [ { value: "Omar Castro <ocastro@gmail.com>",
                                getAttribute: \_ -> "to" },
                              { value: "Gabriela Palacios <gpalacios@gmail.com>",
                                getAttribute: \_ -> "to" },
                              { value: "Cristina Galindo <cgalindo@gmail.com>",
                                getAttribute: \_ -> "to" },
                              { value: "Some Value",
                                getAttribute: \_ -> "something" },
                              { value: "Some Other Value",
                                getAttribute: \_ ->  "something else" } ]
                 _ -> [] }


--- << Mock to test the correct filtering by "name" attibute in <input> tags
nameAttrs :: HtmlElt
nameAttrs = { getElementsByTagName: \_ ->
               [ { value: "Omar Castro <ocastro@gmail.com>",
                   getAttribute: \str -> case str of
                     "name" -> "to"
                     _ -> "Another Value" } ] }


--- <<  Mocks to test the correct filtering depending on the value of the "name" attribute
threeEmails :: HtmlElt
threeEmails = { getElementsByTagName: \_ ->
                 [ { value: "Omar Maturana <omaturana@gmail.com>",
                     getAttribute: \_ -> "to" },
                   { value: "Gabriela Palacios <gpalacios@gmail.com>",
                     getAttribute: \_ -> "to" },
                   { value: "Cristina Galindo <cgalindo@gmail.com>",
                     getAttribute: \_ -> "to" },
                   { value: "Some Value",
                     getAttribute: \_ -> "something" },
                   { value: "Some Other Value",
                     getAttribute: \_ ->  "something else" } ] }


noEmails :: HtmlElt
noEmails = { getElementsByTagName: \_ ->
              [ { value: "Some Value",
                  getAttribute: \_ -> "something" },
                { value: "Some Other Value",
                  getAttribute: \_ ->  "something else" },
                { value: "Some Value",
                  getAttribute: \_ -> "something" },
                { value: "Some Other Value",
                  getAttribute: \_ ->  "something else" } ] }
--- >>


testQueryForInputTag :: QueryEmailTest
testQueryForInputTag =
  it "passes *input* string to the JS function which queries <input> tags" do
    values <- liftEff $ readEmails_ inputTags
    values `shouldNotEqual` []


testQueryForNameAttr :: QueryEmailTest
testQueryForNameAttr =
  it "passes *name* string to the JS function which filters inputs by attribute's value" do
    values <- liftEff $ readEmails_ nameAttrs
    values `shouldNotEqual` []


testFilteringOfInputElements :: QueryEmailTest
testFilteringOfInputElements =
  it "filters out the <input> elements whose *name* attribute does not have value *to*" do
    values <- liftEff $ readEmails_ threeEmails
    noValues <-liftEff $ readEmails_ noEmails
    length values `shouldEqual` 3
    noValues `shouldEqual` []
