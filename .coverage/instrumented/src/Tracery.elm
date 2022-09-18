module Tracery exposing (fromJson, fromSyntax)

import Coverage
import Dict exposing (Dict)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Syntax exposing (Definition(..), Expression(..), Syntax)


fromJson : String -> Result Json.Decode.Error (Generator String)
fromJson string =
    let
        _ =
            Coverage.track "Tracery" 0
    in
    string |> Syntax.fromString |> Result.map fromSyntax


fromSyntax : Syntax -> Generator String
fromSyntax syntax =
    let
        _ =
            Coverage.track "Tracery" 1
    in
    generateStory Syntax.originString Dict.empty syntax |> Random.map Tuple.first


generateStory : String -> Dict String String -> Syntax -> Generator ( String, Dict String String )
generateStory k0 constants syntax =
    let
        _ =
            Coverage.track "Tracery" 12
    in
    Dict.get k0 syntax
        |> Maybe.map
            (\definition ->
                let
                    _ =
                        Coverage.track "Tracery" 11
                in
                case definition of
                    Choose statements ->
                        let
                            _ =
                                Coverage.track "Tracery" 4
                        in
                        case statements of
                            [] ->
                                let
                                    _ =
                                        Coverage.track "Tracery" 2
                                in
                                Random.constant ( "", constants )

                            head :: tail ->
                                let
                                    _ =
                                        Coverage.track "Tracery" 3
                                in
                                Random.uniform head tail
                                    |> Random.andThen (generateSentence constants syntax)

                    Let sentence ->
                        let
                            _ =
                                Coverage.track "Tracery" 8
                        in
                        case constants |> Dict.get k0 of
                            Just string ->
                                let
                                    _ =
                                        Coverage.track "Tracery" 5
                                in
                                Random.constant ( string, constants )

                            Nothing ->
                                let
                                    _ =
                                        Coverage.track "Tracery" 7
                                in
                                sentence
                                    |> generateSentence constants syntax
                                    |> Random.map
                                        (\( s, c ) ->
                                            let
                                                _ =
                                                    Coverage.track "Tracery" 6
                                            in
                                            ( s, c |> Dict.insert k0 s )
                                        )

                    With subSyntax ->
                        let
                            _ =
                                Coverage.track "Tracery" 10
                        in
                        (subSyntax |> Dict.union (syntax |> Dict.remove Syntax.originString))
                            |> generateStory Syntax.originString constants
                            |> Random.map
                                (\( s, c ) ->
                                    let
                                        _ =
                                            Coverage.track "Tracery" 9
                                    in
                                    ( s, c |> Dict.insert k0 s )
                                )
            )
        |> Maybe.withDefault (Random.constant ( "", constants ))


generateSentence : Dict String String -> Syntax -> List Expression -> Generator ( String, Dict String String )
generateSentence constants syntax sentence =
    let
        _ =
            Coverage.track "Tracery" 19
    in
    sentence
        |> List.foldl
            (\exp generator ->
                let
                    _ =
                        Coverage.track "Tracery" 18
                in
                case exp of
                    Print string ->
                        let
                            _ =
                                Coverage.track "Tracery" 14
                        in
                        generator
                            |> Random.map
                                (Tuple.mapFirst
                                    (\it ->
                                        let
                                            _ =
                                                Coverage.track "Tracery" 13
                                        in
                                        it ++ string
                                    )
                                )

                    Insert key ->
                        let
                            _ =
                                Coverage.track "Tracery" 17
                        in
                        generator
                            |> Random.andThen
                                (\( s1, g1 ) ->
                                    let
                                        _ =
                                            Coverage.track "Tracery" 16
                                    in
                                    generateStory key g1 syntax
                                        |> Random.map
                                            (Tuple.mapFirst
                                                (\s2 ->
                                                    let
                                                        _ =
                                                            Coverage.track "Tracery" 15
                                                    in
                                                    s1 ++ s2
                                                )
                                            )
                                )
            )
            (Random.constant ( "", constants ))
