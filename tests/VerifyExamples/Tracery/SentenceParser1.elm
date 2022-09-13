module VerifyExamples.Tracery.SentenceParser1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Tracery exposing (..)
import Parser







spec1 : Test.Test
spec1 =
    Test.test "#sentenceParser: \n\n    \"Hello \\\\\\\\ World \\\\#\"\n    |> Parser.run sentenceParser\n    --> Ok [Print \"Hello \",Print \"\\\\\",Print \" World \",Print \"#\"]" <|
        \() ->
            Expect.equal
                (
                "Hello \\\\ World \\#"
                |> Parser.run sentenceParser
                )
                (
                Ok [Print "Hello ",Print "\\",Print " World ",Print "#"]
                )