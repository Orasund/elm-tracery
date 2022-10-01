module Tracery.Syntax exposing
    ( Expression(..), Definition(..)
    , decoder, fromString, originString
    )

{-| This modules exposes the internal structures of the package.

Its intended to be used in combination with some preprocessing.

@docs Expression, Definition
@docs decoder, fromString, originString

-}

import Dict exposing (Dict)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Set


{-| The expressions always return a string

  - `Value` - just return the given string
  - `Variable` - look up the key and insert a generated string according to the definition of the key.

-}
type Expression
    = Value String
    | Variable String


{-| The definition specifies how the strings gets generated

  - `Choose` - Choose a random sentence out of a list.
  - `Let` - Generate the sentence one. Then use the sentence over and over again.
  - `With` - Generate the sentence according to the sub-grammar.

-}
type Definition
    = Choose (List (List Expression))
    | Let (List Expression)
    | With (Dict String Definition)


{-| The origin is the starting point for the generated story.

    originString : String
    originString =
        "origin"

-}
originString : String
originString =
    "origin"


isValid : List String -> Dict String Definition -> Result String ()
isValid oldKeys dict =
    let
        keys =
            dict |> Dict.keys |> (++) oldKeys

        verify k sentences =
            if
                List.any
                    (\sentenceKeys ->
                        not (List.member k sentenceKeys)
                    )
                    sentences
            then
                sentences
                    |> List.concat
                    |> List.filterMap
                        (\sentenceKey ->
                            if List.member sentenceKey keys then
                                Nothing

                            else
                                Just sentenceKey
                        )
                    |> (\l ->
                            case l of
                                [] ->
                                    Ok ()

                                [ a ] ->
                                    "In the definition of "
                                        ++ k
                                        ++ " the variable "
                                        ++ a
                                        ++ " is not defined. Defined is "
                                        ++ (keys ++ oldKeys |> String.join ", ")
                                        |> Err

                                head :: tail ->
                                    "In the definition of "
                                        ++ k
                                        ++ " the variables "
                                        ++ (tail |> String.join ", ")
                                        ++ " and "
                                        ++ head
                                        ++ " are not defined."
                                        |> Err
                       )

            else
                "The definition of "
                    ++ k
                    ++ " needs an option that does not contain itself."
                    |> Err
    in
    dict
        |> Dict.toList
        |> List.map
            (\( k, v ) ->
                if k /= originString && List.member k oldKeys then
                    Err (k ++ " has already been defined.")

                else
                    case v of
                        Choose l ->
                            l
                                |> List.map
                                    (\sentence ->
                                        sentence
                                            |> List.filterMap
                                                (\exp ->
                                                    case exp of
                                                        Value _ ->
                                                            Nothing

                                                        Variable key ->
                                                            Just key
                                                )
                                    )
                                |> verify k

                        Let l ->
                            l
                                |> List.filterMap
                                    (\exp ->
                                        case exp of
                                            Value _ ->
                                                Nothing

                                            Variable key ->
                                                Just key
                                    )
                                |> List.singleton
                                |> verify k

                        With subSyntax ->
                            isValid keys subSyntax
            )
        |> Result.Extra.combine
        |> Result.map (\_ -> ())


{-|

    import Dict exposing (Dict)
    import Tracery.Command exposing (Command(..))

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

    output : Dict String Definition
    output =
        Dict.fromList
            [ ( "origin"
              , Choose
                  [ [ (Value "Hello "),  (Value "\\"),  (Value " World "), (Value "#")]
                  , [ (Variable "statement"),  (Value " and "),  (Variable "statement")]
                  ]
              )
            , ( "statement"
              , Dict.fromList
                  [ ( "origin"
                    , [  (Value "my ")
                      ,  (Variable "myPet")
                      ,  (Value " is the ")
                      ,  (Variable "complement")
                      ]
                        |> Let
                    )
                  , ( "myPet",Let [ (Variable "pet")])
                  , ( "pet", Choose [[ (Value "cat")],[ (Value "dog")]])
                  , ( "complement"
                    , Choose
                        [ [  (Value "smartest "),  (Variable "myPet"),  (Value " in the world")]
                        , [  (Value "fastest "),  (Variable "myPet"),  (Value " that i know of")]
                        ]
                    )
                  ]
                    |> With
                )
            ]

    input |> fromString
    --> Ok output

-}
fromString : String -> Result Json.Decode.Error (Dict String Definition)
fromString =
    Json.Decode.decodeString decoder


{-| Decoder for the Syntax.
-}
decoder : Json.Decode.Decoder (Dict String Definition)
decoder =
    Json.Value.decoder
        |> Json.Decode.andThen
            (\jsonValue ->
                jsonValue
                    |> decodeSyntax
                    |> Result.andThen
                        (\syntax ->
                            syntax
                                |> isValid []
                                |> Result.map (\() -> syntax)
                        )
                    |> Result.map Json.Decode.succeed
                    |> Result.Extra.extract Json.Decode.fail
            )


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
            |> Parser.map Value
        , Parser.succeed (Value "\\")
            |. Parser.token "\\\\"
        , Parser.succeed (Value "#")
            |. Parser.token "\\#"
        , Parser.succeed Variable
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
