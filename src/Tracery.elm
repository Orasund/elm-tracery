module Tracery exposing (Expression(..), sentenceParser)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Set


type Expression
    = Print String
    | Choose String


type alias Vocabulary =
    Dict String (List (List Expression))


fromJson : Decoder Vocabulary
fromJson =
    Json.Value.decoder
        |> Json.Decode.andThen
            (\jsonValue ->
                case jsonValue of
                    ObjectValue list ->
                        list
                            |> List.map
                                (\( k, v ) ->
                                    case v of
                                        ArrayValue l ->
                                            l
                                                |> List.map
                                                    (\sentence ->
                                                        case sentence of
                                                            StringValue string ->
                                                                string
                                                                    |> Parser.run sentenceParser
                                                                    |> Result.mapError (\_ -> "")

                                                            _ ->
                                                                errorString
                                                                    { expected = "a string"
                                                                    , got = sentence
                                                                    }
                                                                    |> Err
                                                    )
                                                |> Result.Extra.combine
                                                |> Result.map (\ok -> ( k, ok ))

                                        _ ->
                                            errorString
                                                { expected = "a list"
                                                , got = jsonValue
                                                }
                                                |> Err
                                )
                            |> Result.Extra.combine
                            |> (\result ->
                                    case result of
                                        Ok ok ->
                                            ok
                                                |> Dict.fromList
                                                |> Json.Decode.succeed

                                        Err err ->
                                            Json.Decode.fail err
                               )

                    _ ->
                        errorString
                            { expected = "an object"
                            , got = jsonValue
                            }
                            |> Json.Decode.fail
            )
--}


{-|

    import Parser

    "Hello \\\\ World \\#"
    |> Parser.run sentenceParser
    --> Ok [Print "Hello ",Print "\\",Print " World ",Print "#"]

    "My #Pet# is awesome"
    |> Parser.run sentenceParser
    --> Ok [Print "My ", Choose "Pet", Print " is awesome"]

-}
sentenceParser : Parser (List Expression)
sentenceParser =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ Parser.succeed (\stmt -> Parser.Loop (stmt :: list))
                    |= expressionParser
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse list))
                ]
        )


expressionParser : Parser Expression
expressionParser =
    let
        validChar char =
            char /= '#' && char /= '\\'

        variable =
            Parser.variable
                { start = validChar
                , inner = validChar
                , reserved = Set.empty
                }
    in
    Parser.oneOf
        [ variable
            |> Parser.map Print
        , Parser.succeed (Print "\\")
            |. Parser.token "\\\\"
        , Parser.succeed (Print "#")
            |. Parser.token "\\#"
        , Parser.succeed Choose
            |. Parser.token "#"
            |= variable
            |. Parser.token "#"
        ]


errorString : { expected : String, got : JsonValue } -> String
errorString args =
    "expected " ++ args.expected ++ " but got " ++ toString args.got ++ "."


toString : JsonValue -> String
toString jsonValue =
    case jsonValue of
        ObjectValue list ->
            "{ " ++ (list |> List.map (\( k, v ) -> k ++ " : " ++ toString v) |> String.join ", ") ++ " }"

        ArrayValue list ->
            "[ " ++ (list |> List.map toString |> String.join ", ") ++ " ]"

        BoolValue bool ->
            if bool then
                "True"

            else
                "False"

        NullValue ->
            "Null"

        NumericValue float ->
            float |> String.fromFloat

        StringValue string ->
            "\"" ++ string ++ "\""
