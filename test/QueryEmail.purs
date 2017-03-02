module Test.QueryEmail ( dummyTest ) where

import Test.Spec ( describe, it )
import Prelude ( bind, ($) )
import Test.Spec.Assertions ( fail, shouldEqual )
import Test.Spec.Reporter.Console ( consoleReporter )
import Test.Spec.Runner ( run )
import DOM.QueryEmail ( readEmails_, DomMock )


mock :: DomMock
mock = { getElementsByTagName: \_ ->
          [ { value: "Omar Castro <ocastro@gmail.com>",
             getAttribute: \_ -> "to" },
           { value: "Gabriela Palacios <gpalacios@gmail.com>",
             getAttribute: \_ -> "to" },
           { value: "Cristina Galindo <cgalindo@gmail.com>",
             getAttribute: \_ -> "to" },
           { value: "Some Value",
             getAttribute: \_ -> "something" },
           { value: "Some Other Value",
             getAttribute: \_ ->  "something else" } ] }


dummyTest =
  it "1 equals 1" do
    1 `shouldEqual` 1
