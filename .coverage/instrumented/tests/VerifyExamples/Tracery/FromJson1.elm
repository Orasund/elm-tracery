module VerifyExamples.Tracery.FromJson1 exposing (..)

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



spec1 : Test.Test
spec1 =
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": [\"My #favoritePet# is the best #favoritePet# in the world\"]\n    , \"favoritePet\" : \"#pet#\"\n    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]\n    }\n    \"\"\"\n    |> generate 42\n    --> \"My dog is the best dog in the world\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": ["My #favoritePet# is the best #favoritePet# in the world"]
                , "favoritePet" : "#pet#"
                , "pet": ["cat","dog","fish","parrot"]
                }
                """
                |> generate 42
                )
                (
                "My dog is the best dog in the world"
                )