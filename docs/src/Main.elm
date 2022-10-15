module Main exposing (main)

import ElmBook exposing (Book)
import ElmBook.Actions
import ElmBook.Chapter exposing (Chapter)
import ElmBook.Custom exposing (Msg)
import ElmBook.StatefulOptions
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Layout
import Random exposing (Seed)
import Tracery


type alias Model =
    { input : String
    , output : String
    , seed : Seed
    }


init : Model
init =
    { input = "", output = "", seed = Random.initialSeed 42 }
        |> update
            ("{ \"origin\" : \"You can use elm-tracery to generate #idea#\"\n"
                ++ ", \"idea\" :\n"
                ++ "    [ \"a random poem\"\n"
                ++ "    , \"a random movie script\"\n"
                ++ "    , \"a random game level\"\n"
                ++ "    , \"some random flavor text\"\n"
                ++ "    , \"a random drawing\"\n"
                ++ "    , \"a random elm program\"\n"
                ++ "    ]\n"
                ++ "}"
            )


update : String -> Model -> Model
update input model =
    case Tracery.fromJson input of
        Err err ->
            { model
                | input = input
                , output = Json.Decode.errorToString err
            }

        Ok grammar ->
            let
                ( output, seed ) =
                    model.seed
                        |> Random.step (Tracery.run grammar)
            in
            { model
                | input = input
                , output = output
                , seed = seed
            }


firstChapter : Chapter Model
firstChapter =
    ElmBook.Chapter.chapter "Editor"
        |> ElmBook.Chapter.withStatefulComponent component
        |> ElmBook.Chapter.render content


component : Model -> Html (Msg Model)
component model =
    let
        onPress input =
            input
                |> update
                |> ElmBook.Actions.updateState
    in
    [ Html.textarea
        [ Html.Events.onInput onPress
        , Attr.value model.input
        , Attr.style "height" "400px"
        , Attr.class "elm-book-md__code"
        , Attr.style "color" "var(--elm-book-accent)"
        ]
        []
    , [ Html.h2 [] [ Html.text "Output" ]
      , Layout.button
            { onPress = Just (onPress model.input)
            , label = "Generate"
            }
            [ Attr.style "background" "var(--elm-book-background)"
            , Attr.style "width" "fit-content"
            , Attr.style "color" "var(--elm-book-accent)"
            , Attr.style "padding" "8px 20px"
            , Attr.style "border-radius" "4px"
            , Attr.style "height" "fit-content"
            ]
      ]
        |> Layout.row [ Layout.alignCenter, Layout.spacing 8 ]
    , Html.text model.output
    ]
        |> Layout.column [ Layout.spacing 8 ]


content : String
content =
    """
Here you can play around with the language.

<component />
"""


main : Book Model
main =
    ElmBook.book "Elm-tracery"
        |> ElmBook.withStatefulOptions
            [ ElmBook.StatefulOptions.initialState init
            ]
        |> ElmBook.withChapters
            [ firstChapter
            ]
