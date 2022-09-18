module Test1 exposing (main)

import Coverage
import Html
import Json.Decode
import Random
import Tracery


seed : Random.Seed
seed =
    let
        _ =
            Coverage.track "Test1" 0
    in
    Random.initialSeed 42


main =
    let
        _ =
            Coverage.track "Test1" 4
    in
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
                let
                    _ =
                        Coverage.track "Test1" 3
                in
                case result of
                    Err err ->
                        let
                            _ =
                                Coverage.track "Test1" 1
                        in
                        Json.Decode.errorToString err

                    Ok generator ->
                        let
                            _ =
                                Coverage.track "Test1" 2
                        in
                        Random.step generator seed
                            |> Tuple.first
           )
        |> Html.text
