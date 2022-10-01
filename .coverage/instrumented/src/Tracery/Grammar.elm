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

import Coverage
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
    let
        _ =
            Coverage.track "Tracery.Grammar" 0
    in
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
    let
        _ =
            Coverage.track "Tracery.Grammar" 2
    in
    grammar
        |> end
        |> (\g ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 1
                in
                { g | output = [] } |> withCommands g.output
           )


{-| set the remaining commands as output
-}
end : Grammar -> Grammar
end grammar =
    let
        _ =
            Coverage.track "Tracery.Grammar" 3
    in
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
    let
        _ =
            Coverage.track "Tracery.Grammar" 5
    in
    grammar
        |> end
        |> .output
        |> Tracery.Command.toString
            (\variable ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 4
                in
                fun { variable = variable }
            )


{-| Sets Commands of a Grammar.
-}
withCommands : List Command -> Grammar -> Grammar
withCommands trace grammar =
    let
        _ =
            Coverage.track "Tracery.Grammar" 8
    in
    case trace of
        [] ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 6
            in
            { grammar | next = Nothing }

        head :: tail ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 7
            in
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
    let
        _ =
            Coverage.track "Tracery.Grammar" 9
    in
    grammar |> withCommands grammar.stack


{-| Generates a string while a predicate is valid
-}
generateWhile : (Grammar -> Bool) -> Grammar -> Generator Grammar
generateWhile fun grammar =
    let
        _ =
            Coverage.track "Tracery.Grammar" 12
    in
    grammar
        |> generateOutput fun defaultStrategy
        |> Random.andThen
            (while
                (\g ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 10
                    in
                    fun g && Tracery.Command.holes g.output /= []
                )
                (\g ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 11
                    in
                    g
                        |> rewind
                        |> generateOutput fun defaultStrategy
                )
            )


{-| Generates an output found in the resulting grammar.

You can use `generateCommands` instead, If you intend to get the output right away.

    import Dict
    import Json.Decode
    import Random exposing (Generator)
    import Result.Extra
    import Tracery.Syntax exposing (Definition(..), Expression(..))
    import Set

    andThenToString : ({ variable : String } -> String) -> Int -> (Grammar -> Generator Grammar) -> Grammar -> String
    andThenToString fun seed gen grammar =
        Random.step (gen grammar) (Random.initialSeed seed)
            |> Tuple.first
            |> toString fun

    input : Grammar
    input =
        [ ( "origin", Choose [ [ Value "A ", Variable "animal" ] ] )
        , ( "animal"
          , Choose
                [ [ Value "cat, looking at a ", Variable "animal" ]
                , [ Value "bird." ]
                ]
          )
        ]
            |> Dict.fromList
            |> fromDefinitions

    input
    |> andThenToString (\{variable} -> "dog.") 42 (generateOutput (\_ -> True) defaultStrategy)
    --> "A cat, looking at a cat, looking at a cat, looking at a cat, looking at a bird."

    input
    |> andThenToString (\{variable} -> "dog.") 42 (generateOutput (\_ -> True) (noRecursionStrategy (Set.fromList ["animal"])))
    --> "A bird."

-}
generateOutput : (Grammar -> Bool) -> Strategy -> Grammar -> Generator Grammar
generateOutput fun strategy =
    let
        _ =
            Coverage.track "Tracery.Grammar" 14
    in
    while
        (\g ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 13
            in
            fun g && g.next /= Nothing
        )
        (generateNext strategy)


while : (Grammar -> Bool) -> (Grammar -> Generator Grammar) -> Grammar -> Generator Grammar
while fun do init =
    let
        _ =
            Coverage.track "Tracery.Grammar" 18
    in
    do init
        |> Random.andThen
            (\g ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 17
                in
                if fun g then
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 15
                    in
                    while fun do g

                else
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 16
                    in
                    Random.constant g
            )


{-| Computes the command in `grammar.next`.

Afterwards the next command gets loaded

    import Dict
    import Json.Decode
    import Random exposing (Generator)
    import Result.Extra
    import Tracery.Syntax exposing (Definition(..), Expression(..))

    andThenToString : ({ variable : String } -> String) -> Int -> (Grammar -> Generator Grammar) -> Grammar -> String
    andThenToString fun seed gen grammar =
        Random.step (gen grammar) (Random.initialSeed seed)
            |> Tuple.first
            |> toString fun

    input : Grammar
    input =
        [ ( "origin", Choose [ [ Value "A ", Variable "animal" ] ] )
        , ( "animal"
          , Choose
                [ [ Value "cat, looking at a ", Variable "animal" ]
                , [ Value "bird." ]
                ]
          )
        ]
            |> Dict.fromList
            |> fromDefinitions

using this function, you can step through the computation

    input
    |> andThenToString (\{variable} -> "dog.") 42 (generateNext defaultStrategy)
    --> "A dog."

The second step does nothing (some steps only perform internal rearrangements)

    input
    |> andThenToString (\{variable} -> "dog.") 42
        (\g -> g
            |> (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
        )
    --> "A dog."

Doing a second step results in

    input
    |> andThenToString (\{variable} -> "dog.") 42
        (\g -> g
            |> (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
        )
    --> "A cat, looking at a dog."

-}
generateNext : Strategy -> Grammar -> Generator Grammar
generateNext strategy grammar =
    let
        _ =
            Coverage.track "Tracery.Grammar" 27
    in
    (case grammar.next of
        Just next ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 24
            in
            case next of
                Print (Variable k0) ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 19
                    in
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
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 20
                    in
                    Random.constant { grammar | output = grammar.output ++ [ string |> Value |> Print ] }

                Define dict ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 21
                    in
                    { grammar | definitions = grammar.definitions |> Dict.union dict }
                        |> Random.constant

                Delete list ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 22
                    in
                    { grammar | definitions = list |> List.foldl Dict.remove grammar.definitions }
                        |> Random.constant

                Save { asConstant, replaceWith } ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 23
                    in
                    { grammar
                        | constants = grammar.constants |> Dict.insert asConstant grammar.output
                        , output = replaceWith
                    }
                        |> Random.constant

        Nothing ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 25
            in
            Random.constant grammar
    )
        |> Random.map
            (\g ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 26
                in
                { g | output = Tracery.Command.simplify g.output } |> toNext
            )


generateFromDefinition :
    String
    -> Grammar
    -> Strategy
    -> Definition
    -> Generator Grammar
generateFromDefinition k0 grammar strategy definition =
    let
        _ =
            Coverage.track "Tracery.Grammar" 36
    in
    case definition of
        Choose statements ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 31
            in
            (case List.filter (strategy k0) statements of
                [] ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 28
                    in
                    Random.constant []

                head :: tail ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 29
                    in
                    Random.uniform head tail
            )
                |> Random.map
                    (\stack ->
                        let
                            _ =
                                Coverage.track "Tracery.Grammar" 30
                        in
                        { grammar | stack = Tracery.Command.fromExpressions stack ++ grammar.stack }
                    )

        Let statement ->
            let
                _ =
                    Coverage.track "Tracery.Grammar" 34
            in
            case grammar.constants |> Dict.get k0 of
                Just stack ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 32
                    in
                    { grammar | output = grammar.output ++ stack }
                        |> Random.constant

                Nothing ->
                    let
                        _ =
                            Coverage.track "Tracery.Grammar" 33
                    in
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
            let
                _ =
                    Coverage.track "Tracery.Grammar" 35
            in
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
    let
        _ =
            Coverage.track "Tracery.Grammar" 40
    in
    (Set.member key set |> not)
        || List.all
            (\exp ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 39
                in
                case exp of
                    Value _ ->
                        let
                            _ =
                                Coverage.track "Tracery.Grammar" 37
                        in
                        True

                    Variable string ->
                        let
                            _ =
                                Coverage.track "Tracery.Grammar" 38
                        in
                        key /= string
            )
            list


{-| This strategy will only chose recursive options
-}
onlyRecursionStrategy : Set String -> Strategy
onlyRecursionStrategy set key list =
    let
        _ =
            Coverage.track "Tracery.Grammar" 44
    in
    (Set.member key set |> not)
        || List.any
            (\exp ->
                let
                    _ =
                        Coverage.track "Tracery.Grammar" 43
                in
                case exp of
                    Value _ ->
                        let
                            _ =
                                Coverage.track "Tracery.Grammar" 41
                        in
                        True

                    Variable string ->
                        let
                            _ =
                                Coverage.track "Tracery.Grammar" 42
                        in
                        key == string
            )
            list


{-| This strategy will choose any option
-}
defaultStrategy : Strategy
defaultStrategy _ _ =
    let
        _ =
            Coverage.track "Tracery.Grammar" 45
    in
    True
