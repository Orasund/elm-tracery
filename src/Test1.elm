module Test1 exposing (main)

import Tracery
import Random
import Json.Decode
import Html

seed : Random.Seed
seed =
    Random.initialSeed 42

main =
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
        |> Html.text

