module Tracery.Grammar exposing (..)

{-| Creates a string generator based on a syntax.
-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition(..), Expression(..), Syntax)


type alias Grammar =
    { constants : Dict String String
    , definitions : Dict String Definition
    }


mapConstants : (Dict String String -> Dict String String) -> Grammar -> Grammar
mapConstants fun grammar =
    { grammar | constants = fun grammar.constants }


withSyntax : Syntax -> Grammar -> Grammar
withSyntax syntax grammar =
    { grammar | definitions = syntax }


fromSyntax : Syntax -> Grammar
fromSyntax syntax =
    { constants = Dict.empty, definitions = syntax }


generate : Grammar -> Generator String
generate grammar =
    generateStory Tracery.Syntax.originString grammar |> Random.map Tuple.first


generateStory : String -> Grammar -> Generator ( String, Grammar )
generateStory k0 grammar =
    Dict.get k0 grammar.definitions
        |> Maybe.map
            (\definition ->
                case definition of
                    Choose statements ->
                        case statements of
                            [] ->
                                Random.constant ( "", grammar )

                            head :: tail ->
                                Random.uniform head tail
                                    |> Random.andThen (generateSentence grammar)

                    Let sentence ->
                        case grammar.constants |> Dict.get k0 of
                            Just string ->
                                Random.constant ( string, grammar )

                            Nothing ->
                                sentence
                                    |> generateSentence grammar
                                    |> Random.map
                                        (\( s, c ) -> ( s, c |> mapConstants (Dict.insert k0 s) ))

                    With subSyntax ->
                        grammar
                            |> withSyntax (subSyntax |> Dict.union (grammar.definitions |> Dict.remove Tracery.Syntax.originString))
                            |> generateStory Tracery.Syntax.originString
                            |> Random.map (\( s, c ) -> ( s, c |> mapConstants (Dict.insert k0 s) ))
            )
        |> Maybe.withDefault (Random.constant ( "error: " ++ k0 ++ " does not exist", grammar ))


generateSentence : Grammar -> List Expression -> Generator ( String, Grammar )
generateSentence grammar sentence =
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
                                    generateStory key g1
                                        |> Random.map (Tuple.mapFirst (\s2 -> s1 ++ s2))
                                )
            )
            (Random.constant ( "", grammar ))
