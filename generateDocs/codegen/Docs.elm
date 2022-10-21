module Docs exposing (..)

import Block exposing (blockDeclaration)
import Elm exposing (Declaration, File)
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Docs exposing (Module)


fromModules : List Module -> File
fromModules list =
    let
        blocks =
            list
                |> List.map
                    (\m ->
                        Elm.record
                            [ ( "name", Elm.string m.name )
                            , ( "content"
                              , m
                                    |> Elm.Docs.toBlocks
                                    |> List.map Block.fromDocs
                                    |> Elm.list
                              )
                            ]
                    )
                |> Elm.list
    in
    [ Elm.declaration "modules" blocks
    , Elm.comment "--------------------------\n   Types\n--------------------------"
    , Block.typeDeclaration
    , Block.blockDeclaration
    , Block.unionDeclaration
    , Block.aliasDeclaration
    , Block.valueDeclaration
    , Block.associativityDeclaration
    , Block.binopDeclaration
    ]
        |> Elm.file [ "ElmBook", "Gen", "Module" ]
