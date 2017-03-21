module Test.QueryEmail ( testQueryForDivTags
                       , testEmailExtraction ) where


import Prelude ((==), bind, ($), Unit)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)
import DOM.QueryDocument (readEmails, DocumentElement)
import DOM  (DOM)
import Data.Array (length)
import Data.Identity (Identity)
import Test.Spec (it, Group)
import Control.Monad.State.Trans (StateT)
import Test.Spec.Assertions (shouldEqual)
import Data.Maybe (Maybe(..))


type QueryEmailTest = forall eff .
                      StateT (Array (Group (Aff (dom ∷ DOM | eff) Unit))) Identity Unit


--- <<  Mock to test the correct searching of <div> tags with class vR
divTags :: DocumentElement
divTags = { getElementsByClassName: \class_ ->
               case class_ of
                 "vR" -> [ { firstChild: { getAttribute: \attr -> "omaturana@gmail.com" } }
                         , { firstChild: { getAttribute: \attr -> "gpalacios@gmail.com" } } ]
                 _ -> []
          }


--- <<  Mock to test extraction of email attribute from first child of <div class="vR"> tags
emailAttrs :: DocumentElement
emailAttrs = { getElementsByClassName: \_ -> [ { firstChild: { getAttribute: \attr ->
                                                                if attr == "email"
                                                                  then "omaturana@gmail.com"
                                                                  else "NO-EMAIL" } }
                                             , { firstChild: { getAttribute: \attr ->
                                                                if attr == "email"
                                                                  then "gpalacios@gmail.com"
                                                                  else "NO-EMAIL" } } ]
             }


testQueryForDivTags :: QueryEmailTest
testQueryForDivTags =
  it "passes *vR* class name to the JS function which filters divs by class-name" do
    values <- liftEff $ readEmails $ Just divTags
    length values `shouldEqual` 2


testEmailExtraction :: QueryEmailTest
testEmailExtraction =
  it "passes *email* attribute name to the JS function which extracts emails from divs' first child" do
    values <- liftEff $ readEmails $ Just emailAttrs
    values `shouldEqual` ["omaturana@gmail.com", "gpalacios@gmail.com"]
