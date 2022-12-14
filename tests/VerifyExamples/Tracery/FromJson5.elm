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
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": [\"I like cats\",\"I like dogs\"]}\n    \"\"\"\n    |> generate 42\n    --> \"I like cats\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": ["I like cats","I like dogs"]}
                """
                |> generate 42
                )
                (
                "I like cats"
                )