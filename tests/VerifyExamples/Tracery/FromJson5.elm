module VerifyExamples.Tracery.FromJson5 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Tracery exposing (..)
import Result.Extra
import Random
import Json.Decode



generate : Int -> String -> String
generate seed json =
    json
        |> Tracery.fromJson
        |> Result.Extra.unpack
            Json.Decode.errorToString
            (\grammar ->
                Random.step (Tracery.run grammar) (Random.initialSeed seed)
                    |> Tuple.first
            )



spec5 : Test.Test
spec5 =
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": \"The \\\\\\\\# and \\\\\\\\\\\\\\\\ characters need to be escaped.\"}\n    \"\"\"\n    |> generate 42\n    --> \"The # and \\\\ characters need to be escaped.\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": "The \\\\# and \\\\\\\\ characters need to be escaped."}
                """
                |> generate 42
                )
                (
                "The # and \\ characters need to be escaped."
                )