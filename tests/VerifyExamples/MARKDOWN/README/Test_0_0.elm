module VerifyExamples.MARKDOWN.README.Test_0_0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Json.Decode
import Random
import Tracery



seed : Random.Seed
seed =
    Random.initialSeed 41



spec0 : Test.Test
spec0 =
    Test.test "Documentation VerifyExamples: \n\n    \"\"\"\n    { \"origin\":\"I have a #favoriteAnimal# named #favoriteAnimalName# and a #animal# named #name#. I love #favoriteAnimalName# the most. It's the best #favoriteAnimal# in the world.\"\n    , \"favoriteAnimal\" : \n      { \"origin\":\"#color# #animal#\"\n      , \"color\": [\"white\",\"black\",\"brown\"]\n      }\n    , \"favoriteAnimalName\" : \"#name#\"\n    , \"animal\":[\"cat\",\"dog\",\"parrot\"]\n    , \"name\": [\"Johnny\",\"Charlie\",\"Twinkle\",\"Charles\"]\n    }\n    \"\"\"\n    |> Tracery.fromJson\n    |> (\\result ->\n        case result of\n            Err err -> Json.Decode.errorToString err\n            Ok generator ->\n                Random.step generator seed\n                |> Tuple.first\n        )\n    --> \"I have a black dog named Johnny and a parrot named Charles. I love Johnny the most. It's the best black dog in the world.\"" <|
        \() ->
            Expect.equal
                (
                """
                { "origin":"I have a #favoriteAnimal# named #favoriteAnimalName# and a #animal# named #name#. I love #favoriteAnimalName# the most. It's the best #favoriteAnimal# in the world."
                , "favoriteAnimal" : 
                  { "origin":"#color# #animal#"
                  , "color": ["white","black","brown"]
                  }
                , "favoriteAnimalName" : "#name#"
                , "animal":["cat","dog","parrot"]
                , "name": ["Johnny","Charlie","Twinkle","Charles"]
                }
                """
                |> Tracery.fromJson
                |> (\result ->
                    case result of
                        Err err -> Json.Decode.errorToString err
                        Ok generator ->
                            Random.step generator seed
                            |> Tuple.first
                    )
                )
                (
                "I have a black dog named Johnny and a parrot named Charles. I love Johnny the most. It's the best black dog in the world."
                )