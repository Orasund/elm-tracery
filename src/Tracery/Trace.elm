module Tracery.Trace exposing (fillAll, holes, simplify, toString)

import Tracery.Syntax exposing (Expression(..))


holes : List Expression -> List String
holes =
    List.filterMap
        (\exp ->
            case exp of
                Insert string ->
                    Just string

                _ ->
                    Nothing
        )


fillAll : (String -> List Expression) -> List Expression -> List Expression
fillAll fun =
    List.concatMap
        (\exp ->
            case exp of
                Insert string ->
                    fun string

                _ ->
                    exp |> List.singleton
        )


simplify : List Expression -> List Expression
simplify list =
    case list of
        [] ->
            []

        head :: rest ->
            rest
                |> List.foldl
                    (\exp ( h, l ) ->
                        case ( exp, h ) of
                            ( Print a, Print b ) ->
                                ( Print (b ++ a), l )

                            _ ->
                                ( exp, h :: l )
                    )
                    ( head, [] )
                |> (\( a, b ) -> a :: b)


toString : (String -> String) -> List Expression -> String
toString fun list =
    list
        |> List.map
            (\exp ->
                case exp of
                    Insert string ->
                        fun string

                    Print string ->
                        string
            )
        |> String.concat
