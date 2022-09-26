module Tracery.Grammar exposing (..)

{-| Creates a string generator based on a syntax.
-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition(..), Expression(..), Syntax)


{-|

  - `next` - what needs to be generated next?
  - `constants` - what
  - `definitions` - the grammar rules (see Syntax)

-}
type alias Grammar =
    { story : String
    , next : Maybe String
    , constants : Dict String String
    , definitions : Dict String Definition
    }


mapConstants : (Dict String String -> Dict String String) -> Grammar -> Grammar
mapConstants fun grammar =
    { grammar | constants = fun grammar.constants }


withSyntax : Syntax -> Grammar -> Grammar
withSyntax syntax grammar =
    { grammar | definitions = syntax }


fromSyntax : Syntax -> Grammar
fromSyntax syntax =
    { story = ""
    , next = Just Tracery.Syntax.originString
    , constants = Dict.empty
    , definitions = syntax
    }


generate : Grammar -> Generator String
generate grammar =
    generateStory grammar |> Random.map Tuple.first


generateStory : Grammar -> Generator ( String, Maybe Grammar )
generateStory grammar =
    case grammar.next of
        Just k0 ->
            Dict.get k0 grammar.definitions
                |> Maybe.map
                    (\definition ->
                        case definition of
                            Choose statements ->
                                case statements of
                                    [] ->
                                        Random.constant ( "", Just grammar )

                                    head :: tail ->
                                        Random.uniform head tail
                                            |> Random.andThen (generateSentence grammar)

                            Let sentence ->
                                case grammar.constants |> Dict.get k0 of
                                    Just string ->
                                        Random.constant ( string, Just grammar )

                                    Nothing ->
                                        sentence
                                            |> generateSentence grammar
                                            |> Random.map
                                                (\( s, maybe ) ->
                                                    case maybe of
                                                        Just c ->
                                                            ( s, c |> mapConstants (Dict.insert k0 s) |> Just )

                                                        Nothing ->
                                                            ( s, maybe )
                                                )

                            With subDefinitions ->
                                { grammar
                                    | next = Just Tracery.Syntax.originString
                                    , definitions =
                                        subDefinitions
                                            |> Dict.union
                                                (grammar.definitions
                                                    |> Dict.remove Tracery.Syntax.originString
                                                )
                                }
                                    |> generateStory
                                    |> Random.map
                                        (\( s, maybe ) ->
                                            case maybe of
                                                Just c ->
                                                    ( s, c |> mapConstants (Dict.insert k0 s) |> Just )

                                                Nothing ->
                                                    ( s, maybe )
                                        )
                    )
                |> Maybe.withDefault (Random.constant ( "error: " ++ k0 ++ " does not exist", Nothing ))

        Nothing ->
            Random.constant ( grammar.story, Nothing )


generateSentence : Grammar -> List Expression -> Generator ( String, Maybe Grammar )
generateSentence grammar sentence =
    sentence
        |> List.foldl
            (\exp generator ->
                case exp of
                    Print string ->
                        generator |> Random.map (Tuple.mapFirst (\it -> it ++ string))

                    Insert key ->
                        generator
                            |> Random.andThen
                                (\( s1, maybe ) ->
                                    case maybe of
                                        Just g1 ->
                                            generateStory { g1 | next = Just key }
                                                |> Random.map (Tuple.mapFirst (\s2 -> s1 ++ s2))

                                        Nothing ->
                                            Random.constant ( s1, maybe )
                                )
            )
            (Random.constant ( "", Just grammar ))
