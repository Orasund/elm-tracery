module Tracery.Command exposing
    ( Command(..), simplify, toString
    , fillAll, fromExpressions, holes
    )

{-| Commands are used to be able to pause the execution of a Grammar.

By modifying the commands in a grammar you can directly change how the program should run.

@docs Command, simplify, toString

@docs fillAll, fromExpressions, holes

-}

import Coverage
import Dict exposing (Dict)
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition, Expression(..))


type Command
    = Print Expression
    | Define (Dict String Definition)
    | Delete (List String)
    | Save { asConstant : String, replaceWith : List Command }


fromExpressions : List Expression -> List Command
fromExpressions =
    let
        _ =
            Coverage.track "Tracery.Command" 0
    in
    List.map Print


holes : List Command -> List String
holes =
    let
        _ =
            Coverage.track "Tracery.Command" 4
    in
    List.filterMap
        (\exp ->
            let
                _ =
                    Coverage.track "Tracery.Command" 3
            in
            case exp of
                Print (Variable string) ->
                    let
                        _ =
                            Coverage.track "Tracery.Command" 1
                    in
                    Just string

                _ ->
                    let
                        _ =
                            Coverage.track "Tracery.Command" 2
                    in
                    Nothing
        )


fillAll : (String -> Generator (List Command)) -> List Command -> Generator (List Command)
fillAll fun =
    let
        _ =
            Coverage.track "Tracery.Command" 10
    in
    List.foldl
        (\exp ->
            let
                _ =
                    Coverage.track "Tracery.Command" 9
            in
            Random.andThen
                (\list ->
                    let
                        _ =
                            Coverage.track "Tracery.Command" 8
                    in
                    (case exp of
                        Print (Variable string) ->
                            let
                                _ =
                                    Coverage.track "Tracery.Command" 5
                            in
                            fun string

                        _ ->
                            let
                                _ =
                                    Coverage.track "Tracery.Command" 6
                            in
                            exp |> List.singleton |> Random.constant
                    )
                        |> Random.map
                            (\e ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Command" 7
                                in
                                list ++ e
                            )
                )
        )
        (Random.constant [])


simplify : List Command -> List Command
simplify list =
    let
        _ =
            Coverage.track "Tracery.Command" 17
    in
    case list of
        [] ->
            let
                _ =
                    Coverage.track "Tracery.Command" 11
            in
            []

        head :: rest ->
            let
                _ =
                    Coverage.track "Tracery.Command" 16
            in
            rest
                |> List.foldl
                    (\exp ( h, l ) ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 14
                        in
                        case ( exp, h ) of
                            ( Print (Value a), Print (Value b) ) ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Command" 12
                                in
                                ( (b ++ a) |> Value |> Print, l )

                            _ ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Command" 13
                                in
                                ( exp, h :: l )
                    )
                    ( head, [] )
                |> (\( a, b ) ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 15
                        in
                        a :: b
                   )


toString : (String -> String) -> List Command -> String
toString fun list =
    let
        _ =
            Coverage.track "Tracery.Command" 25
    in
    list
        |> List.foldl
            (\cmd out ->
                let
                    _ =
                        Coverage.track "Tracery.Command" 24
                in
                case cmd of
                    Print exp ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 20
                        in
                        case exp of
                            Variable string ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Command" 18
                                in
                                out ++ fun string

                            Value string ->
                                let
                                    _ =
                                        Coverage.track "Tracery.Command" 19
                                in
                                out ++ string

                    Save { replaceWith } ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 21
                        in
                        "[Save]" ++ toString fun replaceWith

                    Define _ ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 22
                        in
                        "[Define]"

                    Delete _ ->
                        let
                            _ =
                                Coverage.track "Tracery.Command" 23
                        in
                        "[Delete]"
            )
            ""
