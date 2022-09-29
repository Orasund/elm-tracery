module Tracery.Trace exposing (Command(..), fillAll, fromExpressions, holes, simplify, toString)

import Dict exposing (Dict)
import Tracery.Syntax exposing (Definition, Expression(..))


type Command
    = Print Expression
    | Define (Dict String Definition)
    | Delete (List String)


fromExpressions : List Expression -> List Command
fromExpressions =
    List.map Print


holes : List Command -> List String
holes =
    List.filterMap
        (\exp ->
            case exp of
                Print (Variable string) ->
                    Just string

                _ ->
                    Nothing
        )


fillAll : (String -> List Command) -> List Command -> List Command
fillAll fun =
    List.concatMap
        (\exp ->
            case exp of
                Print (Variable string) ->
                    fun string

                _ ->
                    exp |> List.singleton
        )


simplify : List Command -> List Command
simplify list =
    case list of
        [] ->
            []

        head :: rest ->
            rest
                |> List.foldl
                    (\exp ( h, l ) ->
                        case ( exp, h ) of
                            ( Print (Value a), Print (Value b) ) ->
                                ( (b ++ a) |> Value |> Print, l )

                            _ ->
                                ( exp, h :: l )
                    )
                    ( head, [] )
                |> (\( a, b ) -> a :: b)


toString : (String -> String) -> List Command -> String
toString fun list =
    list
        |> List.map
            (\cmd ->
                case cmd of
                    Print exp ->
                        case exp of
                            Variable string ->
                                fun string

                            Value string ->
                                string

                    _ ->
                        ""
            )
        |> String.concat
