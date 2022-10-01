module Tracery.Syntax exposing
    ( Expression(..), Definition(..)
    , decoder, fromString, originString
    )

{-| This modules exposes the internal structures of the package.

Its intended to be used in combination with some preprocessing.

@docs Expression, Definition
@docs decoder, fromString, originString

-}

import Coverage
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
    let
        _ =
            Coverage.track "Tracery.Syntax" 0
    in
    "origin"


isValid : List String -> Dict String Definition -> Result String ()
isValid oldKeys dict =
    let
        _ =
            Coverage.track "Tracery.Syntax" 27

        keys =
            let
                _ =
                    Coverage.track "Tracery.Syntax" 1
            in
            dict |> Dict.keys |> (++) oldKeys

        verify k sentences =
            let
                _ =
                    Coverage.track "Tracery.Syntax" 12
            in
            if
                List.any
                    (\sentenceKeys ->
                        let
                            _ =
                                Coverage.track "Tracery.Syntax" 2
                        in
                        not (List.member k sentenceKeys)
                    )
                    sentences
            then
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 10
                in
                sentences
                    |> List.concat
                    |> List.filterMap
                        (\sentenceKey ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 5
                            in
                            if List.member sentenceKey keys then
                                let
                                    _ =
                                        Coverage.track "Tracery.Syntax" 3
                                in
                                Nothing

                            else
                                let
                                    _ =
                                        Coverage.track "Tracery.Syntax" 4
                                in
                                Just sentenceKey
                        )
                    |> (\l ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 9
                            in
                            case l of
                                [] ->
                                    let
                                        _ =
                                            Coverage.track "Tracery.Syntax" 6
                                    in
                                    Ok ()

                                [ a ] ->
                                    let
                                        _ =
                                            Coverage.track "Tracery.Syntax" 7
                                    in
                                    "In the definition of "
                                        ++ k
                                        ++ " the variable "
                                        ++ a
                                        ++ " is not defined. Defined is "
                                        ++ (keys ++ oldKeys |> String.join ", ")
                                        |> Err

                                head :: tail ->
                                    let
                                        _ =
                                            Coverage.track "Tracery.Syntax" 8
                                    in
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
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 11
                in
                "The definition of "
                    ++ k
                    ++ " needs an option that does not contain itself."
                    |> Err
    in
    dict
        |> Dict.toList
        |> List.map
            (\( k, v ) ->
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 25
                in
                if k /= originString && List.member k oldKeys then
                    let
                        _ =
                            Coverage.track "Tracery.Syntax" 13
                    in
                    Err (k ++ " has already been defined.")

                else
                    let
                        _ =
                            Coverage.track "Tracery.Syntax" 24
                    in
                    case v of
                        Choose l ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 18
                            in
                            l
                                |> List.map
                                    (\sentence ->
                                        let
                                            _ =
                                                Coverage.track "Tracery.Syntax" 17
                                        in
                                        sentence
                                            |> List.filterMap
                                                (\exp ->
                                                    let
                                                        _ =
                                                            Coverage.track "Tracery.Syntax" 16
                                                    in
                                                    case exp of
                                                        Value _ ->
                                                            let
                                                                _ =
                                                                    Coverage.track "Tracery.Syntax" 14
                                                            in
                                                            Nothing

                                                        Variable key ->
                                                            let
                                                                _ =
                                                                    Coverage.track "Tracery.Syntax" 15
                                                            in
                                                            Just key
                                                )
                                    )
                                |> verify k

                        Let l ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 22
                            in
                            l
                                |> List.filterMap
                                    (\exp ->
                                        let
                                            _ =
                                                Coverage.track "Tracery.Syntax" 21
                                        in
                                        case exp of
                                            Value _ ->
                                                let
                                                    _ =
                                                        Coverage.track "Tracery.Syntax" 19
                                                in
                                                Nothing

                                            Variable key ->
                                                let
                                                    _ =
                                                        Coverage.track "Tracery.Syntax" 20
                                                in
                                                Just key
                                    )
                                |> List.singleton
                                |> verify k

                        With subSyntax ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 23
                            in
                            isValid keys subSyntax
            )
        |> Result.Extra.combine
        |> Result.map
            (\_ ->
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 26
                in
                ()
            )


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
    let
        _ =
            Coverage.track "Tracery.Syntax" 28
    in
    Json.Decode.decodeString decoder


{-| Decoder for the Syntax.
-}
decoder : Json.Decode.Decoder (Dict String Definition)
decoder =
    let
        _ =
            Coverage.track "Tracery.Syntax" 32
    in
    Json.Value.decoder
        |> Json.Decode.andThen
            (\jsonValue ->
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 31
                in
                jsonValue
                    |> decodeSyntax
                    |> Result.andThen
                        (\syntax ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 30
                            in
                            syntax
                                |> isValid []
                                |> Result.map
                                    (\() ->
                                        let
                                            _ =
                                                Coverage.track "Tracery.Syntax" 29
                                        in
                                        syntax
                                    )
                        )
                    |> Result.map Json.Decode.succeed
                    |> Result.Extra.extract Json.Decode.fail
            )


decodeSyntax : JsonValue -> Result String (Dict String Definition)
decodeSyntax jsonValue =
    let
        _ =
            Coverage.track "Tracery.Syntax" 37
    in
    case jsonValue of
        ObjectValue list ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 35
            in
            list
                |> List.map
                    (\( k, v ) ->
                        let
                            _ =
                                Coverage.track "Tracery.Syntax" 34
                        in
                        decodeDefinition v
                            |> Result.map
                                (\ok ->
                                    let
                                        _ =
                                            Coverage.track "Tracery.Syntax" 33
                                    in
                                    ( k, ok )
                                )
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList

        _ ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 36
            in
            errorString
                { expected = "an object"
                , got = jsonValue
                }
                |> Err


decodeDefinition : JsonValue -> Result String Definition
decodeDefinition jsonValue =
    let
        _ =
            Coverage.track "Tracery.Syntax" 47
    in
    case jsonValue of
        StringValue string ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 39
            in
            string
                |> Parser.run sentenceParser
                |> Result.mapError
                    (\_ ->
                        let
                            _ =
                                Coverage.track "Tracery.Syntax" 38
                        in
                        ""
                    )
                |> Result.map Let

        ArrayValue l ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 44
            in
            l
                |> List.map
                    (\sentence ->
                        let
                            _ =
                                Coverage.track "Tracery.Syntax" 43
                        in
                        case sentence of
                            StringValue string ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Syntax" 41
                                in
                                string
                                    |> Parser.run sentenceParser
                                    |> Result.mapError
                                        (\_ ->
                                            let
                                                _ =
                                                    Coverage.track "Tracery.Syntax" 40
                                            in
                                            ""
                                        )

                            _ ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Syntax" 42
                                in
                                errorString
                                    { expected = "a string"
                                    , got = sentence
                                    }
                                    |> Err
                    )
                |> Result.Extra.combine
                |> Result.map Choose

        ObjectValue _ ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 45
            in
            decodeSyntax jsonValue
                |> Result.map With

        _ ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 46
            in
            errorString
                { expected = "a string, list or object"
                , got = jsonValue
                }
                |> Err


sentenceParser : Parser (List Expression)
sentenceParser =
    let
        _ =
            Coverage.track "Tracery.Syntax" 51
    in
    Parser.loop []
        (\list ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 50
            in
            Parser.oneOf
                [ Parser.succeed
                    (\stmt ->
                        let
                            _ =
                                Coverage.track "Tracery.Syntax" 48
                        in
                        Parser.Loop (stmt :: list)
                    )
                    |= expressionParser
                , Parser.succeed ()
                    |> Parser.map
                        (\_ ->
                            let
                                _ =
                                    Coverage.track "Tracery.Syntax" 49
                            in
                            Parser.Done (List.reverse list)
                        )
                ]
        )


expressionParser : Parser Expression
expressionParser =
    let
        _ =
            Coverage.track "Tracery.Syntax" 54

        validChar char =
            let
                _ =
                    Coverage.track "Tracery.Syntax" 52
            in
            char /= '#' && char /= '\\'

        variable =
            let
                _ =
                    Coverage.track "Tracery.Syntax" 53
            in
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
    let
        _ =
            Coverage.track "Tracery.Syntax" 55
    in
    "expected " ++ args.expected ++ " but got " ++ toString args.got ++ "."


toString : JsonValue -> String
toString jsonValue =
    let
        _ =
            Coverage.track "Tracery.Syntax" 65
    in
    case jsonValue of
        ObjectValue list ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 57
            in
            "{ "
                ++ (list
                        |> List.map
                            (\( k, v ) ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Syntax" 56
                                in
                                k ++ " : " ++ toString v
                            )
                        |> String.join ", "
                   )
                ++ " }"

        ArrayValue list ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 58
            in
            "[ " ++ (list |> List.map toString |> String.join ", ") ++ " ]"

        BoolValue bool ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 61
            in
            if bool then
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 59
                in
                "True"

            else
                let
                    _ =
                        Coverage.track "Tracery.Syntax" 60
                in
                "False"

        NullValue ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 62
            in
            "Null"

        NumericValue float ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 63
            in
            float |> String.fromFloat

        StringValue string ->
            let
                _ =
                    Coverage.track "Tracery.Syntax" 64
            in
            "\"" ++ string ++ "\""
