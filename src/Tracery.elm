module Tracery exposing
    ( fromJson, run, runTo
    , step, stepNonRecursive, stepOnlyRecursive
    , toString
    )

{-| Tracery is a text-generation language mostly used for twitter bots.

See [Tracery.io](www.tracery.io) for more information.

@docs fromJson, run, runTo

@docs step, stepNonRecursive, stepOnlyRecursive

-}

import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Set
import Tracery.Command exposing (Command(..))
import Tracery.Grammar exposing (Grammar)
import Tracery.Syntax exposing (Definition(..), Expression(..))


{-| Turns a tracery json-string into a generator

    import Json.Decode
    import Random
    import Result.Extra

    generate : Int -> String -> String
    generate seed json =
        json
            |> Tracery.fromJson
            |> Result.Extra.unpack
                Json.Decode.errorToString
                (\grammar ->
                    Random.step (Tracery.run grammar) (Random.initialSeed seed)
                        |> Tuple.first
                )

A tracery json is a object that has a `origin` field.

The `#` and `\` characters need to be escaped.

    """
    { "origin": "The \\\\# and \\\\\\\\ characters need to be escaped."}
    """
    |> generate 42
    --> "The # and \\ characters need to be escaped."

If you provide a list, tracer will tick an element at random.

    """
    { "origin": ["I like cats","I like dogs"]}
    """
    |> generate 42
    --> "I like cats"

You can reference other fields using `#..#`

    """
    { "origin": ["I have two pets: a #pet# and a #pet#"]
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate 42
    --> "I have two pets: a dog and a cat"

Definitions may also be recursive.

    """
    { "origin": ["I have #pets#"]
    , "pets": ["a #pet#","a #pet# and #pets#"]
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate 20
    --> "I have a fish and a cat and a dog"

You can define constants by providing a string instead of a list.

    """
    { "origin": ["My #favoritePet# is the best #favoritePet# in the world"]
    , "favoritePet" : "#pet#"
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate 42
    --> "My dog is the best dog in the world"

You may define sub-definitions to organize your definitions.

    """
    { "origin": ["My #cat#","My #dog#"]
    , "cat":
      { "origin":"cat is named #name#"
      , "name": ["Cleopatra","Cesar"]
      }
    , "dog":
      { "origin":"dog is named #name#"
      , "name": ["Athena","Zeus"]
      }
    }
    """
    |> generate 42
    --> "My cat is named Cleopatra"

-}
fromJson : String -> Result Json.Decode.Error Grammar
fromJson string =
    string |> Tracery.Syntax.fromString |> Result.map Tracery.Grammar.fromDefinitions


{-| Runs a grammar until it ends.

Some recursive definitions might take a long time.

Use `runTo` if you want to avoid long waiting times.

-}
run : Grammar -> Generator String
run grammar =
    grammar
        |> Tracery.Grammar.generateWhile (\_ -> True)
        |> Random.map (Tracery.Grammar.toString (\_ -> ""))


{-| Will just write the current output.

use run or runTo, to actually compute something.

-}
toString : ({ variable : String } -> String) -> Grammar -> String
toString fun =
    Tracery.Grammar.toString fun


{-| Runs a grammar until it reaches a key in the list.

    import Json.Decode
    import Random
    import Result.Extra

    generateTo : List String -> ({variable:String} -> String)-> Int -> String -> String
    generateTo list fun seed json =
        json
            |> Tracery.fromJson
            |> Result.Extra.unpack
                Json.Decode.errorToString
                (\grammar ->
                    Random.step (Tracery.runTo list grammar) (Random.initialSeed seed)
                        |> Tuple.first
                        |> Debug.log "debug"
                        |> toString fun
                )

    """
    { "origin": ["A #color# #animal#"]
    , "color": ["black","white","gray"]
    , "animal":
      [ "cat, looking at a #color# #animal#"
      , "bird."
      ]
    }
    """
    |> generateTo ["animal"] (\{variable} -> "dog.") 42
    --> "A black dog."

-}
runTo : List String -> Grammar -> Generator Grammar
runTo list =
    let
        set =
            Set.fromList list
    in
    Tracery.Grammar.generateWhile
        (\g ->
            case g.next of
                Just (Print (Variable string)) ->
                    not (Set.member string set)

                _ ->
                    True
        )


{-| compute a single step.

    import Json.Decode
    import Random exposing (Generator)
    import Result.Extra
    import Tracery.Grammar exposing (Grammar)

    andThenToString : ({ variable : String } -> String) -> Int -> (Grammar -> Generator Grammar) -> Result Json.Decode.Error Grammar -> String
    andThenToString fun seed gen =
        Result.Extra.unpack
            Json.Decode.errorToString
            (\grammar ->
                Random.step (gen grammar) (Random.initialSeed seed)
                    |> Tuple.first
                    |> toString fun
            )

    input : Result Json.Decode.Error Grammar
    input =
        """
        { "origin": ["A #animal#"]
        , "animal":
        [ "cat, looking at a #animal#"
        , "bird."
        ]
        }
        """
            |> Tracery.fromJson

using this function, you can step through the computation

    input
    |> andThenToString (\{variable} -> "dog.") 42 Tracery.step
    --> "A dog."

The second step does nothing (some steps only perform internal rearrangements)

    input
    |> andThenToString (\{variable} -> "dog.") 42
        (\g -> g |> Tracery.step |> Random.andThen Tracery.step )
    --> "A dog."

Doing a second step results in

    input
    |> andThenToString (\{variable} -> "dog.") 42
        (\g -> g
            |> Tracery.step
            |> Random.andThen Tracery.step
            |> Random.andThen Tracery.step
        )
    --> "A cat, looking at a dog."

-}
step : Grammar -> Generator Grammar
step =
    Tracery.Grammar.generateNext Tracery.Grammar.defaultStrategy


{-| compute a single step.

The algorithm will never choose a recursive option for any key in the given list.

-}
stepNonRecursive : List String -> Grammar -> Generator Grammar
stepNonRecursive list =
    list
        |> Set.fromList
        |> Tracery.Grammar.noRecursionStrategy
        |> Tracery.Grammar.generateNext


{-| compute a single step.

The algorithm will always choose a recursive option for any key in the given list.

If no such option exists, then the empty list will be used instead.

-}
stepOnlyRecursive : List String -> Grammar -> Generator Grammar
stepOnlyRecursive list =
    list
        |> Set.fromList
        |> Tracery.Grammar.onlyRecursionStrategy
        |> Tracery.Grammar.generateNext
