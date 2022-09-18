module Tracery exposing (fromJson, fromSyntax)

import Dict exposing (Dict)
import Json.Decode
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Syntax exposing (Definition(..), Expression(..), Syntax)


{-| Turns a tracery json-string into a generator

A tracery json is a object that has a `origin` field.

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
    string |> Syntax.fromString |> Result.map fromSyntax


{-| Creates a string generator based on a syntax.
-}
fromSyntax : Syntax -> Generator String
fromSyntax syntax =
    generateStory Syntax.originString Dict.empty syntax |> Random.map Tuple.first


generateStory : String -> Dict String String -> Syntax -> Generator ( String, Dict String String )
generateStory k0 constants syntax =
    Dict.get k0 syntax
        |> Maybe.map
            (\definition ->
                case definition of
                    Choose statements ->
                        case statements |> Debug.log "statements" of
                            [] ->
                                Random.constant ( "", constants )

                            head :: tail ->
                                Random.uniform head tail
                                    |> Random.andThen (generateSentence constants syntax)

                    Let sentence ->
                        case constants |> Dict.get k0 of
                            Just string ->
                                Random.constant ( string, constants )

                            Nothing ->
                                sentence
                                    |> generateSentence constants syntax
                                    |> Random.map (\( s, c ) -> ( s, c |> Dict.insert k0 s ))

                    With subSyntax ->
                        (subSyntax |> Dict.union (syntax |> Dict.remove Syntax.originString))
                            |> generateStory Syntax.originString constants
                            |> Random.map (\( s, c ) -> ( s, c |> Dict.insert k0 s ))
            )
        |> Maybe.withDefault (Random.constant ( "error: " ++ k0 ++ " does not exist", constants ))


generateSentence : Dict String String -> Syntax -> List Expression -> Generator ( String, Dict String String )
generateSentence constants syntax sentence =
    sentence
        |> List.foldl
            (\exp generator ->
                case exp of
                    Print string ->
                        generator |> Random.map (Tuple.mapFirst (\it -> it ++ string))

                    Insert key ->
                        generator
                            |> Random.andThen
                                (\( s1, g1 ) ->
                                    generateStory key g1 syntax
                                        |> Debug.log "story"
                                        |> Random.map (Tuple.mapFirst (\s2 -> s1 ++ s2))
                                )
            )
            (Random.constant ( "", constants ))
