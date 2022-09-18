module VerifyExamples.Tracery.FromJson2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Tracery exposing (..)
import Result.Extra
import Random
import Json.Decode



generate : String -> String
generate json =
    json
        |> Tracery.fromJson
        |> Result.Extra.unpack
            Json.Decode.errorToString
            (\generator ->
                Random.step generator seed
                    |> Tuple.first
            )
seed : Random.Seed
seed =
    Random.initialSeed 40



spec2 : Test.Test
spec2 =
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": \"I have #pets#\"\n    , \"pets\": [\"a #pet#\",\"a #pet# and #pets#\"]\n    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]\n    }\n    \"\"\"\n    |> generate\n    --> \"I have a cat and a dog\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": "I have #pets#"
                , "pets": ["a #pet#","a #pet# and #pets#"]
                , "pet": ["cat","dog","fish","parrot"]
                }
                """
                |> generate
                )
                (
                "I have a cat and a dog"
                )