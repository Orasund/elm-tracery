module Tracery exposing
    ( fromJson, run, runTo
    , step, stepNonRecursive, stepOnlyRecursive
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
import Tracery.Grammar exposing (Grammar)
import Tracery.Syntax exposing (Definition(..), Expression(..))
import Tracery.Trace exposing (Command(..))


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
run =
    Tracery.Grammar.generateWhile (\_ -> True)


{-| Runs a grammar until it reaches a key in the list.
-}
runTo : List String -> Grammar -> Generator String
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
