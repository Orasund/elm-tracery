module Tracery exposing (fromJson, fromSyntax)

import Dict exposing (Dict)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Syntax exposing (Definition(..), Expression(..), Syntax)


fromJson : String -> Result Json.Decode.Error (Generator String)
fromJson string =
    string |> Syntax.fromString |> Result.map fromSyntax


fromSyntax : Syntax -> Generator String
fromSyntax syntax =
    generateStory Syntax.originString Dict.empty syntax |> Random.map Tuple.first


generateStory : String -> Dict String String -> Syntax -> Generator ( String, Dict String String )
generateStory k0 constants syntax =
    Dict.get k0 syntax
        |> Maybe.map
            (\definition ->
                case definition of
                    Choose statements ->
                        case statements of
                            [] ->
                                Random.constant ( "", constants )

                            head :: tail ->
                                Random.uniform head tail
                                    |> Random.andThen (generateSentence constants syntax)

                    Let sentence ->
                        case constants |> Dict.get k0 of
                            Just string ->
                                Random.constant ( string, constants )

                            Nothing ->
                                sentence
                                    |> generateSentence constants syntax
                                    |> Random.map (\(s,c) -> (s,c |> Dict.insert k0 s))

                    With subSyntax ->
                        (subSyntax |> Dict.union (syntax |> Dict.remove Syntax.originString))
                        |> generateStory Syntax.originString constants 
                            |> Random.map  (\(s,c) -> (s,c |> Dict.insert k0 s))
            )
        |> Maybe.withDefault (Random.constant ( "", constants ))


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
