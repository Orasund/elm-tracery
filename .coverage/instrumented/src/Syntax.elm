module Syntax exposing (Definition(..), Expression(..), Syntax, decoder, fromString, originString)

import Coverage
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


originString : String
originString =
    let
        _ =
            Coverage.track "Syntax" 0
    in
    "origin"


isValid : List String -> Syntax -> Result String ()
isValid oldKeys dict =
    let
        _ =
            Coverage.track "Syntax" 27

        keys =
            let
                _ =
                    Coverage.track "Syntax" 1
            in
            dict |> Debug.log "dict" |> Dict.keys |> (++) oldKeys |> Debug.log "keys"

        verify k sentences =
            let
                _ =
                    Coverage.track "Syntax" 12
            in
            if
                List.any
                    (\sentenceKeys ->
                        let
                            _ =
                                Coverage.track "Syntax" 2
                        in
                        not (List.member k sentenceKeys)
                    )
                    sentences
            then
                let
                    _ =
                        Coverage.track "Syntax" 10
                in
                sentences
                    |> List.concat
                    |> List.filterMap
                        (\sentenceKey ->
                            let
                                _ =
                                    Coverage.track "Syntax" 5
                            in
                            if List.member sentenceKey keys then
                                let
                                    _ =
                                        Coverage.track "Syntax" 3
                                in
                                Nothing

                            else
                                let
                                    _ =
                                        Coverage.track "Syntax" 4
                                in
                                Just sentenceKey
                        )
                    |> (\l ->
                            let
                                _ =
                                    Coverage.track "Syntax" 9
                            in
                            case l of
                                [] ->
                                    let
                                        _ =
                                            Coverage.track "Syntax" 6
                                    in
                                    Ok ()

                                [ a ] ->
                                    let
                                        _ =
                                            Coverage.track "Syntax" 7
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
                                            Coverage.track "Syntax" 8
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
                        Coverage.track "Syntax" 11
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
                        Coverage.track "Syntax" 25
                in
                if k /= originString && List.member k oldKeys then
                    let
                        _ =
                            Coverage.track "Syntax" 13
                    in
                    Err (k ++ " has already been defined.")

                else
                    let
                        _ =
                            Coverage.track "Syntax" 24
                    in
                    case v of
                        Choose l ->
                            let
                                _ =
                                    Coverage.track "Syntax" 18
                            in
                            l
                                |> List.map
                                    (\sentence ->
                                        let
                                            _ =
                                                Coverage.track "Syntax" 17
                                        in
                                        sentence
                                            |> List.filterMap
                                                (\exp ->
                                                    let
                                                        _ =
                                                            Coverage.track "Syntax" 16
                                                    in
                                                    case exp of
                                                        Print _ ->
                                                            let
                                                                _ =
                                                                    Coverage.track "Syntax" 14
                                                            in
                                                            Nothing

                                                        Insert key ->
                                                            let
                                                                _ =
                                                                    Coverage.track "Syntax" 15
                                                            in
                                                            Just key
                                                )
                                    )
                                |> verify k

                        Let l ->
                            let
                                _ =
                                    Coverage.track "Syntax" 22
                            in
                            l
                                |> List.filterMap
                                    (\exp ->
                                        let
                                            _ =
                                                Coverage.track "Syntax" 21
                                        in
                                        case exp of
                                            Print _ ->
                                                let
                                                    _ =
                                                        Coverage.track "Syntax" 19
                                                in
                                                Nothing

                                            Insert key ->
                                                let
                                                    _ =
                                                        Coverage.track "Syntax" 20
                                                in
                                                Just key
                                    )
                                |> List.singleton
                                |> verify k

                        With subSyntax ->
                            let
                                _ =
                                    Coverage.track "Syntax" 23
                            in
                            isValid keys subSyntax
            )
        |> Result.Extra.combine
        |> Result.map
            (\_ ->
                let
                    _ =
                        Coverage.track "Syntax" 26
                in
                ()
            )


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

    output : Syntax
    output =
        Dict.fromList
            [ ( "origin"
              , Choose
                  [ [Print "Hello ", Print "\\", Print " World ",Print "#"]
                  , [Insert "statement", Print " and ", Insert "statement"]
                  ]
              )
            , ( "statement"
              , Dict.fromList
                  [ ( "origin"
                    , Let [ Print "my ", Insert "myPet", Print " is the ", Insert "complement"]
                    )
                  , ( "myPet",Let [Insert "pet"])
                  , ( "pet", Choose [[Print "cat"],[Print "dog"]])
                  , ( "complement"
                    , Choose
                        [ [ Print "smartest ", Insert "myPet", Print " in the world"]
                        , [ Print "fastest ", Insert "myPet", Print " that i know of"]
                        ]
                    )
                  ]
                    |> With
                )
            ]

    input |> fromString
    --> Ok output

-}
fromString : String -> Result Json.Decode.Error Syntax
fromString =
    let
        _ =
            Coverage.track "Syntax" 28
    in
    Json.Decode.decodeString decoder


decoder : Json.Decode.Decoder Syntax
decoder =
    let
        _ =
            Coverage.track "Syntax" 32
    in
    Json.Value.decoder
        |> Json.Decode.andThen
            (\jsonValue ->
                let
                    _ =
                        Coverage.track "Syntax" 31
                in
                jsonValue
                    |> decodeSyntax
                    |> Result.andThen
                        (\syntax ->
                            let
                                _ =
                                    Coverage.track "Syntax" 30
                            in
                            syntax
                                |> isValid []
                                |> Result.map
                                    (\() ->
                                        let
                                            _ =
                                                Coverage.track "Syntax" 29
                                        in
                                        syntax
                                    )
                        )
                    |> Result.map Json.Decode.succeed
                    |> Result.Extra.extract Json.Decode.fail
            )


decodeSyntax : JsonValue -> Result String Syntax
decodeSyntax jsonValue =
    let
        _ =
            Coverage.track "Syntax" 37
    in
    case jsonValue of
        ObjectValue list ->
            let
                _ =
                    Coverage.track "Syntax" 35
            in
            list
                |> List.map
                    (\( k, v ) ->
                        let
                            _ =
                                Coverage.track "Syntax" 34
                        in
                        decodeDefinition v
                            |> Result.map
                                (\ok ->
                                    let
                                        _ =
                                            Coverage.track "Syntax" 33
                                    in
                                    ( k, ok )
                                )
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList

        _ ->
            let
                _ =
                    Coverage.track "Syntax" 36
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
            Coverage.track "Syntax" 47
    in
    case jsonValue of
        StringValue string ->
            let
                _ =
                    Coverage.track "Syntax" 39
            in
            string
                |> Parser.run sentenceParser
                |> Result.mapError
                    (\_ ->
                        let
                            _ =
                                Coverage.track "Syntax" 38
                        in
                        ""
                    )
                |> Result.map Let

        ArrayValue l ->
            let
                _ =
                    Coverage.track "Syntax" 44
            in
            l
                |> List.map
                    (\sentence ->
                        let
                            _ =
                                Coverage.track "Syntax" 43
                        in
                        case sentence of
                            StringValue string ->
                                let
                                    _ =
                                        Coverage.track "Syntax" 41
                                in
                                string
                                    |> Parser.run sentenceParser
                                    |> Result.mapError
                                        (\_ ->
                                            let
                                                _ =
                                                    Coverage.track "Syntax" 40
                                            in
                                            ""
                                        )

                            _ ->
                                let
                                    _ =
                                        Coverage.track "Syntax" 42
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
                    Coverage.track "Syntax" 45
            in
            decodeSyntax jsonValue
                |> Result.map With

        _ ->
            let
                _ =
                    Coverage.track "Syntax" 46
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
            Coverage.track "Syntax" 51
    in
    Parser.loop []
        (\list ->
            let
                _ =
                    Coverage.track "Syntax" 50
            in
            Parser.oneOf
                [ Parser.succeed
                    (\stmt ->
                        let
                            _ =
                                Coverage.track "Syntax" 48
                        in
                        Parser.Loop (stmt :: list)
                    )
                    |= expressionParser
                , Parser.succeed ()
                    |> Parser.map
                        (\_ ->
                            let
                                _ =
                                    Coverage.track "Syntax" 49
                            in
                            Parser.Done (List.reverse list)
                        )
                ]
        )


expressionParser : Parser Expression
expressionParser =
    let
        _ =
            Coverage.track "Syntax" 54

        validChar char =
            let
                _ =
                    Coverage.track "Syntax" 52
            in
            char /= '#' && char /= '\\'

        variable =
            let
                _ =
                    Coverage.track "Syntax" 53
            in
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
    let
        _ =
            Coverage.track "Syntax" 55
    in
    "expected " ++ args.expected ++ " but got " ++ toString args.got ++ "."


toString : JsonValue -> String
toString jsonValue =
    let
        _ =
            Coverage.track "Syntax" 65
    in
    case jsonValue of
        ObjectValue list ->
            let
                _ =
                    Coverage.track "Syntax" 57
            in
            "{ "
                ++ (list
                        |> List.map
                            (\( k, v ) ->
                                let
                                    _ =
                                        Coverage.track "Syntax" 56
                                in
                                k ++ " : " ++ toString v
                            )
                        |> String.join ", "
                   )
                ++ " }"

        ArrayValue list ->
            let
                _ =
                    Coverage.track "Syntax" 58
            in
            "[ " ++ (list |> List.map toString |> String.join ", ") ++ " ]"

        BoolValue bool ->
            let
                _ =
                    Coverage.track "Syntax" 61
            in
            if bool then
                let
                    _ =
                        Coverage.track "Syntax" 59
                in
                "True"

            else
                let
                    _ =
                        Coverage.track "Syntax" 60
                in
                "False"

        NullValue ->
            let
                _ =
                    Coverage.track "Syntax" 62
            in
            "Null"

        NumericValue float ->
            let
                _ =
                    Coverage.track "Syntax" 63
            in
            float |> String.fromFloat

        StringValue string ->
            let
                _ =
                    Coverage.track "Syntax" 64
            in
            "\"" ++ string ++ "\""
