module Tracery.Grammar exposing (..)

{-| Creates a string generator based on a syntax.
-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition(..), Expression(..), Syntax)
import Tracery.Trace exposing (Command(..))


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


mapConstants : (Dict String (List Command) -> Dict String (List Command)) -> Grammar -> Grammar
mapConstants fun grammar =
    { grammar | constants = fun grammar.constants }


withSyntax : Syntax -> Grammar -> Grammar
withSyntax syntax grammar =
    { grammar | definitions = syntax }


fromSyntax : Syntax -> Grammar
fromSyntax syntax =
    { output = []
    , stack = []
    , next = Just (Print (Variable Tracery.Syntax.originString))
    , constants = Dict.empty
    , definitions = syntax
    }


withTrace : List Command -> Grammar -> Grammar
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


generateTrace : Grammar -> Generator (List Command)
generateTrace grammar =
    grammar
        |> generateOutput
        |> Random.map .output


generateOutput : Grammar -> Generator Grammar
generateOutput grammar =
    generateNext grammar
        |> Random.andThen
            (\g ->
                if g.next == Nothing then
                    Random.constant g

                else
                    g
                        |> generateOutput
            )


generateNext : Grammar -> Generator Grammar
generateNext grammar =
    (case grammar.next of
        Just next ->
            case next of
                Print (Variable k0) ->
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
                                                    |> Random.map
                                                        (\stack ->
                                                            { grammar | stack = Tracery.Trace.fromExpressions stack ++ grammar.stack }
                                                        )

                                    Let statement ->
                                        case grammar.constants |> Dict.get k0 of
                                            Just stack ->
                                                { grammar | stack = stack ++ grammar.stack }
                                                    |> Random.constant

                                            Nothing ->
                                                { grammar | output = [] }
                                                    |> withTrace (Tracery.Trace.fromExpressions statement)
                                                    |> generateOutput
                                                    |> Random.map
                                                        (\g ->
                                                            { grammar
                                                                | stack = g.output ++ grammar.stack
                                                                , constants =
                                                                    g.constants
                                                                        |> Dict.insert k0 g.output
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
                                            |> generateOutput
                                            |> Random.map
                                                (\g ->
                                                    { grammar
                                                        | output = grammar.output ++ g.output
                                                        , definitions =
                                                            grammar.definitions
                                                                |> Dict.union g.definitions
                                                        , constants =
                                                            grammar.constants
                                                                |> Dict.insert k0 g.output
                                                    }
                                                )
                            )
                        |> Maybe.withDefault (Random.constant { grammar | output = [ "error: " ++ k0 ++ " does not exist" |> Value |> Print ] })

                Print (Value string) ->
                    Random.constant { grammar | output = grammar.output ++ [ string |> Value |> Print ] }

                Define dict ->
                    { grammar | definitions = grammar.definitions |> Dict.union dict }
                        |> Random.constant

                Delete list ->
                    { grammar | definitions = list |> List.foldl Dict.remove grammar.definitions }
                        |> Random.constant

        Nothing ->
            Random.constant grammar
    )
        |> Random.map (\g -> { g | output = Tracery.Trace.simplify g.output } |> toNext)
