module Tracery.Grammar exposing
    ( Grammar, fromDefinitions
    , generateWhile, generateOutput, generateNext
    , Strategy, defaultStrategy, noRecursionStrategy, onlyRecursionStrategy
    , toNext, withCommands
    , end, rewind, toString
    )

{-| Creates a string generator based on a syntax.


# Grammar

@docs Grammar, fromDefinitions


# Generating

@docs generateWhile, generateOutput, generateCommands, generateNext


# Strategy

@docs Strategy, defaultStrategy, noRecursionStrategy, onlyRecursionStrategy


# Technical Utilities

@docs toNext, withCommands

-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Random exposing (Generator)
import Set exposing (Set)
import Tracery.Command exposing (Command(..))
import Tracery.Syntax exposing (Definition(..), Expression(..))


{-|

  - `next` - what needs to be generated next?
  - `constants` - what
  - `definitions` - the grammar rules (see Syntax)

-}
type alias Grammar =
    { output : List Command
    , stack : List Command
    , next : Maybe Command
    , constants : Dict String (List Command)
    , definitions : Dict String Definition
    }


{-| Turns Definitions into a Grammar.
-}
fromDefinitions : Dict String Definition -> Grammar
fromDefinitions syntax =
    { output = []
    , stack = []
    , next = Just (Print (Variable Tracery.Syntax.originString))
    , constants = Dict.empty
    , definitions = syntax
    }


{-| sets the output as input.
-}
rewind : Grammar -> Grammar
rewind grammar =
    grammar
        |> end
        |> (\g -> { g | output = [] } |> withCommands g.output)


{-| set the remaining commands as output
-}
end : Grammar -> Grammar
end grammar =
    { grammar
        | output =
            grammar.output
                ++ (grammar.next
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )
                ++ grammar.stack
        , next = Nothing
        , stack = []
    }


{-| Prints the Grammar
-}
toString : ({ variable : String } -> String) -> Grammar -> String
toString fun grammar =
    grammar
        |> end
        |> .output
        |> Tracery.Command.toString (\variable -> fun { variable = variable })


{-| Sets Commands of a Grammar.
-}
withCommands : List Command -> Grammar -> Grammar
withCommands trace grammar =
    case trace of
        [] ->
            { grammar | next = Nothing }

        head :: tail ->
            { grammar
                | next = Just head
                , stack = tail
            }


{-| Moves to the next command without executing anything.

    toNext : Grammar -> Grammar
    toNext grammar =
        grammar |> withCommands grammar.stack

-}
toNext : Grammar -> Grammar
toNext grammar =
    grammar |> withCommands grammar.stack


{-| Generates a string while a predicate is valid
-}
generateWhile : (Grammar -> Bool) -> Grammar -> Generator Grammar
generateWhile fun grammar =
    grammar
        |> generateOutput fun defaultStrategy
        |> Random.andThen
            (while (\g -> fun g && Tracery.Command.holes g.output /= [])
                (\g ->
                    g
                        |> rewind
                        |> generateOutput fun defaultStrategy
                )
            )


{-| Generates an output found in the resulting grammar.

You can use `generateCommands` instead, If you intend to get the output right away.

-}
generateOutput : (Grammar -> Bool) -> Strategy -> Grammar -> Generator Grammar
generateOutput fun strategy =
    while (\g -> fun g && g.next /= Nothing)
        (\g ->
            let
                _ =
                    g
                        |> end
                        |> .output
                        |> Tracery.Command.toString (\var -> "[var " ++ var ++ "]")
                        |> Debug.log "output"
            in
            g
                |> generateNext strategy
        )


while : (Grammar -> Bool) -> (Grammar -> Generator Grammar) -> Grammar -> Generator Grammar
while fun do init =
    do init
        |> Random.andThen
            (\g ->
                if fun g then
                    while fun do g

                else
                    Random.constant g
            )


{-| Computes the command in `grammar.next`.

Afterwards the next command gets loaded

-}
generateNext : Strategy -> Grammar -> Generator Grammar
generateNext strategy grammar =
    (case grammar.next of
        Just next ->
            case next of
                Print (Variable k0) ->
                    Dict.get k0 grammar.definitions
                        |> Maybe.map
                            (generateFromDefinition k0
                                grammar
                                strategy
                            )
                        |> Maybe.withDefault
                            ({ grammar | output = [ "error: " ++ k0 ++ " does not exist" |> Value |> Print ] }
                                |> Random.constant
                            )

                Print (Value string) ->
                    Random.constant { grammar | output = grammar.output ++ [ string |> Value |> Print ] }

                Define dict ->
                    { grammar | definitions = grammar.definitions |> Dict.union dict }
                        |> Random.constant

                Delete list ->
                    { grammar | definitions = list |> List.foldl Dict.remove grammar.definitions }
                        |> Random.constant

                Save { asConstant, replaceWith } ->
                    { grammar
                        | constants = grammar.constants |> Dict.insert asConstant grammar.output
                        , output = replaceWith
                    }
                        |> Random.constant

        Nothing ->
            Random.constant grammar
    )
        |> Random.map (\g -> { g | output = Tracery.Command.simplify g.output } |> toNext)


generateFromDefinition :
    String
    -> Grammar
    -> Strategy
    -> Definition
    -> Generator Grammar
generateFromDefinition k0 grammar strategy definition =
    case definition of
        Choose statements ->
            (case List.filter (strategy k0) statements of
                [] ->
                    Random.constant []

                head :: tail ->
                    Random.uniform head tail
            )
                |> Random.map
                    (\stack ->
                        { grammar | stack = Tracery.Command.fromExpressions stack ++ grammar.stack }
                    )

        Let statement ->
            case grammar.constants |> Dict.get k0 of
                Just stack ->
                    { grammar | output = grammar.output ++ stack }
                        |> Random.constant

                Nothing ->
                    Random.constant
                        { grammar
                            | output = []
                            , stack =
                                Tracery.Command.fromExpressions statement
                                    ++ [ Save { asConstant = k0, replaceWith = grammar.output }
                                       , Print (Variable k0)
                                       ]
                                    ++ grammar.stack
                        }

        With subDefinitions ->
            Random.constant
                { grammar
                    | stack =
                        [ Tracery.Syntax.originString |> Variable |> Print
                        , subDefinitions |> Dict.keys |> Delete
                        ]
                            ++ grammar.stack
                    , definitions =
                        subDefinitions
                            |> Dict.union
                                (grammar.definitions
                                    |> Dict.remove Tracery.Syntax.originString
                                )
                }



-----------------------------------------------------------------------------------------------------
-- Strategies
-----------------------------------------------------------------------------------------------------


{-| The strategy specifies the algorithm to choose an option
-}
type alias Strategy =
    String -> List Expression -> Bool


{-| This strategy will never choose a recursive option
-}
noRecursionStrategy : Set String -> Strategy
noRecursionStrategy set key list =
    Set.member key set
        && List.all
            (\exp ->
                case exp of
                    Value _ ->
                        True

                    Variable string ->
                        key /= string
            )
            list


{-| This strategy will only chose recursive options
-}
onlyRecursionStrategy : Set String -> Strategy
onlyRecursionStrategy set key list =
    Set.member key set
        && List.any
            (\exp ->
                case exp of
                    Value _ ->
                        True

                    Variable string ->
                        key == string
            )
            list


{-| This strategy will choose any option
-}
defaultStrategy : Strategy
defaultStrategy _ _ =
    True
