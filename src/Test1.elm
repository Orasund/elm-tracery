module Test1 exposing (main)

import Html
import Json.Decode
import Random
import Tracery


seed : Random.Seed
seed =
    Random.initialSeed 42


main =
    """
    { "origin": "I have #pets#"
    , "pets": ["a #pet#","a #pet# and #pets#"]
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
        |> Tracery.fromJson
        |> (\result ->
                case result of
                    Err err ->
                        Json.Decode.errorToString err

                    Ok generator ->
                        Random.step generator seed
                            |> Tuple.first
           )
        |> Html.text
