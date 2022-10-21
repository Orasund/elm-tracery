module ElmBook.Docs exposing (..)

import ElmBook
import ElmBook.Chapter exposing (Chapter)
import ElmBook.Gen.Module
import ElmBook.Docs.View.Block

chapters : List (Chapter msg)
chapters =
    ElmBook.Gen.Module.modules
                |> List.map
                    (\{ name, content } ->
                        ElmBook.Chapter.chapter name
                            |> ElmBook.Chapter.render
                                (content
                                    |> List.map ElmBook.Docs.View.Block.view
                                    |> String.join "\n\n"
                                )
                    )

main =
    ElmBook.book "Documentation"
        |> ElmBook.withChapters (chapters)
