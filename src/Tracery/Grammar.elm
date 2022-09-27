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
    , stack : List Expression
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
    , stack = []
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
                , stack = tail
            }


toNext : Grammar -> Grammar
toNext grammar =
    grammar |> withTrace grammar.stack


generate : Grammar -> Generator String
generate grammar =
    generateTrace grammar
        |> Random.map (Tracery.Trace.toString (\_ -> ""))


generateTrace : Grammar -> Generator (List Expression)
generateTrace grammar =
    grammar
        |> generateStory
        |> Random.map .story


generateStory : Grammar -> Generator Grammar
generateStory grammar =
    generateNext grammar
        |> Random.andThen
            (\g ->
                if g.next == Nothing then
                    Random.constant g

                else
                    g
                        |> generateStory
            )


generateNext : Grammar -> Generator Grammar
generateNext grammar =
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
                                                    |> Random.map (\stack -> { grammar | stack = stack ++ grammar.stack })

                                    Let statement ->
                                        case grammar.constants |> Dict.get k0 of
                                            Just stack ->
                                                { grammar | stack = stack ++ grammar.stack }
                                                    |> Random.constant

                                            Nothing ->
                                                { grammar | story = [] }
                                                    |> withTrace statement
                                                    |> generateStory
                                                    |> Random.map
                                                        (\g ->
                                                            { grammar
                                                                | stack = g.story ++ grammar.stack
                                                                , constants =
                                                                    g.constants
                                                                        |> Dict.insert k0 g.story
                                                            }
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
                                                    { grammar
                                                        | story = grammar.story ++ g.story
                                                        , definitions =
                                                            grammar.definitions
                                                                |> Dict.union g.definitions
                                                        , constants =
                                                            grammar.constants
                                                                |> Dict.insert k0 g.story
                                                    }
                                                )
                            )
                        |> Maybe.withDefault (Random.constant { grammar | story = [ "error: " ++ k0 ++ " does not exist" |> Print ] })

                Print string ->
                    Random.constant { grammar | story = grammar.story ++ [ Print string ] }

        Nothing ->
            Random.constant grammar
    )
        |> Random.map (\g -> { g | story = Tracery.Trace.simplify g.story } |> toNext)
