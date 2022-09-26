module Tracery exposing (fromJson, fromSyntax)

{-| Tracery is a text-generation language mostly used for twitter bots.

See [Tracery.io](www.tracery.io) for more information.

@docs fromJson, fromSyntax

-}

import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Grammar
import Tracery.Syntax exposing (Definition(..), Expression(..), Syntax)


{-| Turns a tracery json-string into a generator

    import Json.Decode
    import Random
    import Result.Extra

    seed : Random.Seed
    seed =
        Random.initialSeed 40

    generate : String -> String
    generate json =
        json
            |> Tracery.fromJson
            |> Result.Extra.unpack
                Json.Decode.errorToString
                (\generator ->
                    Random.step generator seed
                        |> Tuple.first
                )

A tracery json is a object that has a `origin` field.

The `#` and `\` characters need to be escaped.

    """
    { "origin": "The \\\\# and \\\\\\\\ characters need to be escaped."}
    """
    |> generate
    |> Debug.log "exposing"
    --> "The # and \\ characters need to be escaped."

You can reference other fields using `#..#`

    """
    { "origin": "I have two pets: a #pet# and a #pet#"
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate
    --> "I have two pets: a fish and a cat"

Definitions may also be recursive.

    """
    { "origin": "I have #pets#"
    , "pets": ["a #pet#","a #pet# and #pets#"]
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate
    --> "I have a cat and a dog"

You can define constants by providing a string instead of a list.

    """
    { "origin": "My #favoritePet# is the best #favoritePet# in the world"
    , "favoritePet" : "#pet#"
    , "pet": ["cat","dog","fish","parrot"]
    }
    """
    |> generate
    --> "My fish is the best fish in the world"

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
    |> generate
    --> "My dog is named Athena"

-}
fromJson : String -> Result Json.Decode.Error (Generator String)
fromJson string =
    string |> Tracery.Syntax.fromString |> Result.map fromSyntax


{-| Creates a string generator based on a syntax.
-}
fromSyntax : Syntax -> Generator String
fromSyntax syntax =
    Tracery.Grammar.fromSyntax syntax
        |> Tracery.Grammar.generate
