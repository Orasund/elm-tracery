module VerifyExamples.Tracery.FromJson0 exposing (..)

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



spec0 : Test.Test
spec0 =
    Test.test "#fromJson: \n\n    \"\"\"\n    { \"origin\": [\"My #cat#\",\"My #dog#\"]\n    , \"cat\":\n      { \"origin\":\"cat is named #name#\"\n      , \"name\": [\"Cleopatra\",\"Cesar\"]\n      }\n    , \"dog\":\n      { \"origin\":\"dog is named #name#\"\n      , \"name\": [\"Athena\",\"Zeus\"]\n      }\n    }\n    \"\"\"\n    |> generate 42\n    --> \"My cat is named Cleopatra\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin": ["My #cat#","My #dog#"]
                , "cat":
                  { "origin":"cat is named #name#"
                  , "name": ["Cleopatra","Cesar"]
                  }
                , "dog":
                  { "origin":"dog is named #name#"
                  , "name": ["Athena","Zeus"]
                  }
                }
                """
                |> generate 42
                )
                (
                "My cat is named Cleopatra"
                )