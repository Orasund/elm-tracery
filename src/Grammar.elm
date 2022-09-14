module Grammar exposing (Definition(..), Expression(..), Grammar(..), Syntax, decoder, fromString)

import Dict exposing (Dict)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Set


type Expression
    = Print String
    | Insert String


type Definition
    = Choose (List (List Expression))
    | Let (List Expression)
    | With Syntax


type alias Syntax =
    Dict String Definition


type Grammar
    = Grammar
        { constant : Dict String (List Expression)
        , variable : Dict String (List (List Expression))
        , subGrammar : Dict String Grammar
        }


{-|

    import Dict

    input : String
    input =
       """{ "origin" : [ "Hello \\\\\\\\ World \\\\#", "#statement# and #statement#" ]
       , "statement" :
         { "origin" : "my #myPet# is the #complement#"
         , "myPet": "#pet#"
         , "pet" : ["cat","dog"]
         , "complement" : ["smartest #myPet# in the world","fastest #myPet# that i know of"]
         }
       }"""

    output : Grammar
    output =
          Grammar
            { constant = Dict.empty
            , variable = Dict.fromList
                [ ( "origin"
                  , [ [Print "Hello ", Print "\\", Print " World ",Print "#"]
                    , [Insert "statement", Print " and ", Insert "statement"]
                    ]
                  )
                ]
            , subGrammar =
                Dict.fromList
                [   ( "statement"
                    , Grammar
                      { constant = Dict.fromList
                        [ ( "origin"
                          , [ Print "my ", Insert "myPet", Print " is the ", Insert "complement"]
                          )
                        , ( "myPet", [Insert "pet"])
                        ]
                      , variable = Dict.fromList
                        [ ( "pet", [[Print "cat"],[Print "dog"]])
                        , ( "complement"
                          , [ [ Print "smartest ", Insert "myPet", Print " in the world"]
                            , [ Print "fastest ", Insert "myPet", Print " that i know of"]
                            ]
                          )
                        ]
                      , subGrammar = Dict.empty
                      }
                    )
                ]
            }


    input |> fromString
    --> Ok output

-}
fromString : String -> Result Json.Decode.Error Grammar
fromString =
    Json.Decode.decodeString decoder


decoder : Json.Decode.Decoder Grammar
decoder =
    Json.Value.decoder
        |> Json.Decode.andThen
            (\jsonValue ->
                jsonValue
                    |> decodeSyntax
                    |> Result.map Json.Decode.succeed
                    |> Result.Extra.extract Json.Decode.fail
            )
        |> Json.Decode.map fromSyntax


fromSyntax : Syntax -> Grammar
fromSyntax syntax =
    syntax
        |> Dict.foldl
            (\k v dict ->
                case v of
                    Choose list ->
                        { dict | variable = dict.variable |> Dict.insert k list }

                    Let list ->
                        { dict | constant = dict.constant |> Dict.insert k list }

                    With subSyntax ->
                        { dict | subGrammar = dict.subGrammar |> Dict.insert k (fromSyntax subSyntax) }
            )
            { constant = Dict.empty
            , variable = Dict.empty
            , subGrammar = Dict.empty
            }
        |> Grammar


decodeSyntax : JsonValue -> Result String (Dict String Definition)
decodeSyntax jsonValue =
    case jsonValue of
        ObjectValue list ->
            list
                |> List.map
                    (\( k, v ) ->
                        decodeDefinition v
                            |> Result.map (\ok -> ( k, ok ))
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList

        _ ->
            errorString
                { expected = "an object"
                , got = jsonValue
                }
                |> Err


decodeDefinition : JsonValue -> Result String Definition
decodeDefinition jsonValue =
    case jsonValue of
        StringValue string ->
            string
                |> Parser.run sentenceParser
                |> Result.mapError (\_ -> "")
                |> Result.map Let

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
                |> Result.map Choose

        ObjectValue _ ->
            decodeSyntax jsonValue
                |> Result.map With

        _ ->
            errorString
                { expected = "a string, list or object"
                , got = jsonValue
                }
                |> Err


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
        , Parser.succeed Insert
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
