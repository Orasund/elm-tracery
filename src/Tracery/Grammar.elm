module Tracery.Grammar exposing (..)

{-| Creates a string generator based on a syntax.
-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition(..), Expression(..), Syntax)
import Tracery.Trace


{-|

  - `next` - what needs to be generated next?
  - `constants` - what
  - `definitions` - the grammar rules (see Syntax)

-}
type alias Grammar =
    { story : List Expression
    , next : Maybe Expression
    , constants : Dict String (List Expression)
    , definitions : Dict String Definition
    }


mapConstants : (Dict String (List Expression) -> Dict String (List Expression)) -> Grammar -> Grammar
mapConstants fun grammar =
    { grammar | constants = fun grammar.constants }


withSyntax : Syntax -> Grammar -> Grammar
withSyntax syntax grammar =
    { grammar | definitions = syntax }


fromSyntax : Syntax -> Grammar
fromSyntax syntax =
    { story = []
    , next = Just (Insert Tracery.Syntax.originString)
    , constants = Dict.empty
    , definitions = syntax
    }


withTrace : List Expression -> Grammar -> Grammar
withTrace trace grammar =
    case trace of
        [] ->
            { grammar | next = Nothing }

        head :: tail ->
            { grammar
                | next = Just head
                , story = grammar.story ++ tail
            }


generate : Grammar -> Generator String
generate grammar =
    generateTrace grammar
        |> Random.map (Tracery.Trace.toString (\_ -> ""))


generateTrace : Grammar -> Generator (List Expression)
generateTrace grammar =
    generateStory grammar
        |> Random.andThen
            (\g ->
                if g.next == Nothing then
                    Random.constant g.story

                else
                    g
                        |> withTrace g.story
                        |> generateTrace
            )


generateStory : Grammar -> Generator Grammar
generateStory grammar =
    (case grammar.next of
        Just next ->
            case next of
                Insert k0 ->
                    Dict.get k0 grammar.definitions
                        |> Maybe.map
                            (\definition ->
                                case definition of
                                    Choose statements ->
                                        case statements of
                                            [] ->
                                                Random.constant grammar

                                            head :: tail ->
                                                Random.uniform head tail
                                                    |> Random.andThen (generateSentence { grammar | story = [] })
                                                    |> Random.map (\g -> { g | story = grammar.story ++ g.story })

                                    Let sentence ->
                                        case grammar.constants |> Dict.get k0 of
                                            Just trace ->
                                                Random.constant { grammar | story = grammar.story ++ trace }

                                            Nothing ->
                                                sentence
                                                    |> generateSentence { grammar | story = [] }
                                                    |> Random.map
                                                        (\g ->
                                                            { g | story = grammar.story ++ g.story }
                                                                |> mapConstants
                                                                    (g.story
                                                                        |> Dict.insert k0
                                                                    )
                                                        )

                                    With subDefinitions ->
                                        subDefinitions
                                            |> Dict.union
                                                (grammar.definitions
                                                    |> Dict.remove Tracery.Syntax.originString
                                                )
                                            |> fromSyntax
                                            |> (\g -> { g | constants = grammar.constants })
                                            |> generateStory
                                            |> Random.map
                                                (\g ->
                                                    { g | story = grammar.story ++ g.story }
                                                        |> mapConstants (Dict.insert k0 g.story)
                                                )
                            )
                        |> Maybe.withDefault (Random.constant { grammar | story = [ "error: " ++ k0 ++ " does not exist" |> Print ] })

                Print string ->
                    Random.constant { grammar | story = grammar.story ++ [ Print string ] }

        Nothing ->
            Random.constant grammar
    )
        |> Random.map (\g -> { g | next = Nothing })


generateSentence : Grammar -> List Expression -> Generator Grammar
generateSentence grammar sentence =
    sentence
        |> List.foldl
            (\exp generator ->
                generator
                    |> Random.andThen
                        (\g1 ->
                            generateStory { g1 | story = [], next = Just exp }
                                |> Random.map (\g2 -> { g2 | story = g1.story ++ g2.story })
                        )
            )
            (Random.constant { grammar | story = [] })
