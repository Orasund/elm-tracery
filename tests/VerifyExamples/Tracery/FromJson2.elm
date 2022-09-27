module VerifyExamples.Tracery.FromJson2 exposing (..)

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
            (\generator ->
                Random.step generator (Random.initialSeed seed)
                    |> Tuple.first
            )



spec2 : Test.Test
spec2 =
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": [\"I have #pets#\"]\n    , \"pets\": [\"a #pet#\",\"a #pet# and #pets#\"]\n    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]\n    }\n    \"\"\"\n    |> generate 20\n    --> \"I have a fish and a cat and a dog\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": ["I have #pets#"]
                , "pets": ["a #pet#","a #pet# and #pets#"]
                , "pet": ["cat","dog","fish","parrot"]
                }
                """
                |> generate 20
                )
                (
                "I have a fish and a cat and a dog"
                )