module Tracery exposing (fromJson)

import Dict exposing (Dict)
import Grammar exposing (Definition(..), Expression(..), Grammar(..), Syntax)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=), Parser)
import Random exposing (Generator)
import Result.Extra
import Set


fromJson : String -> Result Json.Decode.Error Grammar
fromJson =
    Grammar.fromString


toGenerator : Syntax -> Generator String
toGenerator syntax =
    generateStory "origin" Dict.empty syntax |> Random.map Tuple.first


generateStory : String -> Dict String String -> Syntax -> Generator ( String, Dict String String )
generateStory k0 constants syntax =
    Dict.get k0 syntax
        |> Maybe.map
            (\definition ->
                case definition of
                    Choose statements ->
                        case statements of
                            [] ->
                                Random.constant ""

                            head :: tail ->
                                Random.uniform head tail
                                    |> Random.andThen (generateSentence constants syntax)

                    Let sentence ->
                        case constants |> Dict.get k0 of
                            Just string ->
                                Random.constant string

                            Nothing ->
                                sentence
                                    |> generateSentence constants syntax
            )


generateSentence : Dict String String -> Syntax -> List Expression -> Generator ( String, Dict String String )
generateSentence constants syntax sentence =
    sentence
        |> List.foldl
            (\exp generator ->
                case exp of
                    Print string ->
                        generator |> Random.map (Tuple.mapFirst (\it -> it ++ string))

                    Insert key ->
                        generator
                            |> Random.andThen
                                (\( s1, g1 ) ->
                                    generateStory key g1 syntax
                                        |> Random.map (Tuple.mapFirst (\s2 -> s1 ++ s2))
                                )
            )
            (Random.constant ( "", constants ))
