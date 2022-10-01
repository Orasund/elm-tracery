module Tracery.Command exposing
    ( Command(..), simplify, toString
    , fillAll, fromExpressions, holes
    )

{-| Commands are used to be able to pause the execution of a Grammar.

By modifying the commands in a grammar you can directly change how the program should run.

@docs Command, simplify, toString

@docs fillAll, fromExpressions, holes

-}

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


fillAll : (String -> Generator (List Command)) -> List Command -> Generator (List Command)
fillAll fun =
    List.foldl
        (\exp ->
            Random.andThen
                (\list ->
                    (case exp of
                        Print (Variable string) ->
                            fun string

                        _ ->
                            exp |> List.singleton |> Random.constant
                    )
                        |> Random.map (\e -> list ++ e)
                )
        )
        (Random.constant [])


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
        |> List.foldl
            (\cmd out ->
                case cmd of
                    Print exp ->
                        case exp of
                            Variable string ->
                                out ++ fun string

                            Value string ->
                                out ++ string

                    Save { replaceWith } ->
                        "[Save]" ++ toString fun replaceWith

                    Define _ ->
                        "[Define]"

                    Delete _ ->
                        "[Delete]"
            )
            ""
