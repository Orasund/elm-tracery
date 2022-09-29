module Tracery.Grammar exposing (..)

{-| Creates a string generator based on a syntax.
-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue(..))
import Parser exposing ((|.), (|=))
import Random exposing (Generator)
import Tracery.Syntax exposing (Definition(..), Expression(..))
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


withSyntax : Dict String Definition -> Grammar -> Grammar
withSyntax syntax grammar =
    { grammar | definitions = syntax }


fromSyntax : Dict String Definition -> Grammar
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
                                                Random.constant
                                                    { grammar
                                                        | output = []
                                                        , stack =
                                                            Tracery.Trace.fromExpressions statement
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
        |> Random.map (\g -> { g | output = Tracery.Trace.simplify g.output } |> toNext)
