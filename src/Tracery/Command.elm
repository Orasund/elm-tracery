module Tracery.Command exposing
    ( Command(..), simplify, toString
    , fillAll, fromExpressions, variables
    )

{-| Commands are used to be able to pause the execution of a Grammar.

By modifying the commands in a grammar you can directly change how the program should run.

@docs Command, simplify, toString

@docs fillAll, fromExpressions, variables

-}

import Dict exposing (Dict)
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition, Expression(..))


{-| Defines commands that the algorithm recognizes

  - Print - print the expression to the output
  - Define - Add a set of definitions
  - Delete - Delete a set of definitions
  - Save - saves the current value of the output as a constant and replaces it with a different value.

-}
type Command
    = Print Expression
    | Define (Dict String Definition)
    | Delete (List String)
    | Save { asConstant : String, replaceWith : List Command }


{-| Convert expressions to commands
-}
fromExpressions : List Expression -> List Command
fromExpressions =
    List.map Print


{-| Returns all variables.
-}
variables : List Command -> List String
variables =
    List.filterMap
        (\exp ->
            case exp of
                Print (Variable string) ->
                    Just string

                _ ->
                    Nothing
        )


{-| replaces all variables
-}
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


{-| simplifies the commands.
-}
simplify : List Command -> List Command
simplify list =
    (case list of
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
    )
        |> List.reverse


{-| Turns the list of commands into a readable string
-}
toString : (String -> String) -> List Command -> String
toString f l =
    let
        rec ( out, consts ) fun list =
            case list of
                [] ->
                    out

                cmd :: tail ->
                    case cmd of
                        Print exp ->
                            case exp of
                                Variable string ->
                                    consts
                                        |> Dict.get string
                                        |> Maybe.withDefault (fun string)
                                        |> (\s -> rec ( out ++ s, consts ) fun tail)

                                Value string ->
                                    rec
                                        ( out ++ string
                                        , consts
                                        )
                                        fun
                                        tail

                        Save { asConstant, replaceWith } ->
                            rec ( "", consts |> Dict.insert asConstant out ) fun (replaceWith ++ tail)

                        Define _ ->
                            rec ( out, consts ) fun tail

                        Delete _ ->
                            rec ( out, consts ) fun tail
    in
    rec ( "", Dict.empty ) f l
