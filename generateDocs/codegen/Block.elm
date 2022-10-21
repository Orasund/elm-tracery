module Block exposing (aliasDeclaration, associativityDeclaration, binopDeclaration, blockDeclaration, fromDocs, typeDeclaration, unionDeclaration, valueDeclaration)

import Elm exposing (Declaration, Expression)
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Docs
import Elm.Type


typeAnnotation : Annotation
typeAnnotation =
    Annotation.named [] "Type"


typeDeclaration : Declaration
typeDeclaration =
    [ Elm.variantWith "Var" [ Annotation.string ]
    , Elm.variantWith "Lambda" [ typeAnnotation, typeAnnotation ]
    , Elm.variantWith "Tuple" [ Annotation.list typeAnnotation ]
    , Elm.variantWith "Type"
        [ Annotation.string
        , Annotation.list typeAnnotation
        ]
    , Elm.variantWith "Record"
        [ Annotation.tuple Annotation.string typeAnnotation
            |> Annotation.list
        , Annotation.maybe Annotation.string
        ]
    ]
        |> Elm.customType "Type"


unionDeclaration : Declaration
unionDeclaration =
    [ ( "name", Annotation.string )
    , ( "comment", Annotation.string )
    , ( "args", Annotation.list Annotation.string )
    , ( "tags"
      , Annotation.tuple Annotation.string
            (Annotation.list typeAnnotation)
            |> Annotation.list
      )
    ]
        |> Annotation.record
        |> Elm.alias "Union"


aliasDeclaration : Declaration
aliasDeclaration =
    [ ( "name", Annotation.string )
    , ( "comment", Annotation.string )
    , ( "args", Annotation.list Annotation.string )
    , ( "type_", typeAnnotation )
    ]
        |> Annotation.record
        |> Elm.alias "Alias"


valueDeclaration : Declaration
valueDeclaration =
    [ ( "name", Annotation.string )
    , ( "comment", Annotation.string )
    , ( "type_", typeAnnotation )
    ]
        |> Annotation.record
        |> Elm.alias "Value"


associativityDeclaration : Declaration
associativityDeclaration =
    [ Elm.variant "Left"
    , Elm.variant "None"
    , Elm.variant "Right"
    ]
        |> Elm.customType "Associativity"


binopDeclaration : Declaration
binopDeclaration =
    [ ( "name", Annotation.string )
    , ( "comment", Annotation.string )
    , ( "type_", typeAnnotation )
    , ( "associativity", Annotation.named [] "Associativity" )
    , ( "precedence", Annotation.int )
    ]
        |> Annotation.record
        |> Elm.alias "Binop"


blockDeclaration : Declaration
blockDeclaration =
    [ Elm.variantWith "MarkdownBlock" [ Annotation.string ]
    , Elm.variantWith "UnionBlock" [ Annotation.named [] "Union" ]
    , Elm.variantWith "AliasBlock" [ Annotation.named [] "Alias" ]
    , Elm.variantWith "ValueBlock" [ Annotation.named [] "Value" ]
    , Elm.variantWith "BinopBlock" [ Annotation.named [] "Binop" ]
    , Elm.variantWith "UnknownBlock" [ Annotation.string ]
    ]
        |> Elm.customType "Block"


typeFromDocs : Elm.Type.Type -> Expression
typeFromDocs type_ =
    case type_ of
        Elm.Type.Var string ->
            Elm.apply (Elm.val "Var")
                [ Elm.string string ]

        Elm.Type.Lambda t1 t2 ->
            Elm.apply (Elm.val "Lambda")
                [ typeFromDocs t1
                , typeFromDocs t2
                ]

        Elm.Type.Tuple list ->
            Elm.apply (Elm.val "Tuple")
                [ list
                    |> List.map typeFromDocs
                    |> Elm.list
                ]

        Elm.Type.Type string list ->
            Elm.apply (Elm.val "Type")
                [ Elm.string string
                , list
                    |> List.map typeFromDocs
                    |> Elm.list
                ]

        Elm.Type.Record list maybe ->
            Elm.apply (Elm.val "Record")
                [ list
                    |> List.map
                        (\( string, t ) ->
                            Elm.tuple (Elm.string string)
                                (typeFromDocs t)
                        )
                    |> Elm.list
                , maybe
                    |> Maybe.map Elm.string
                    |> Elm.maybe
                ]


unionFromDocs : Elm.Docs.Union -> Expression
unionFromDocs union =
    Elm.record
        [ ( "name", Elm.string union.name )
        , ( "comment", union.comment |> escape |> Elm.string )
        , ( "args"
          , union.args
                |> List.map Elm.string
                |> Elm.list
          )
        , ( "tags"
          , union.tags
                |> List.map
                    (\( string, list ) ->
                        Elm.tuple
                            (Elm.string string)
                            (list
                                |> List.map typeFromDocs
                                |> Elm.list
                            )
                    )
                |> Elm.list
          )
        ]


aliasFromDocs : Elm.Docs.Alias -> Expression
aliasFromDocs alias_ =
    Elm.record
        [ ( "name", Elm.string alias_.name )
        , ( "comment", alias_.comment |> escape |> Elm.string )
        , ( "args"
          , alias_.args
                |> List.map Elm.string
                |> Elm.list
          )
        , ( "type_", typeFromDocs alias_.tipe )
        ]


valueFromDocs : Elm.Docs.Value -> Expression
valueFromDocs value =
    Elm.record
        [ ( "name", Elm.string value.name )
        , ( "comment", value.comment |> escape |> Elm.string )
        , ( "type_", typeFromDocs value.tipe )
        ]


associativityFromDocs : Elm.Docs.Associativity -> Expression
associativityFromDocs associativity =
    case associativity of
        Elm.Docs.Left ->
            Elm.val "Left"

        Elm.Docs.None ->
            Elm.val "None"

        Elm.Docs.Right ->
            Elm.val "Right"


binopFromDocs : Elm.Docs.Binop -> Expression
binopFromDocs binop =
    Elm.record
        [ ( "name", Elm.string binop.name )
        , ( "comment", binop.comment |> escape |> Elm.string )
        , ( "type_", typeFromDocs binop.tipe )
        , ( "associativity", associativityFromDocs binop.associativity )
        , ( "precedence", Elm.int binop.precedence )
        ]


escape : String -> String
escape string =
    string
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""


fromDocs : Elm.Docs.Block -> Expression
fromDocs block =
    (case block of
        Elm.Docs.MarkdownBlock string ->
            Elm.apply (Elm.val "MarkdownBlock")
                [ string |> escape |> Elm.string ]

        Elm.Docs.UnionBlock union ->
            Elm.apply (Elm.val "UnionBlock")
                [ union |> unionFromDocs ]

        Elm.Docs.AliasBlock alias_ ->
            Elm.apply (Elm.val "AliasBlock")
                [ alias_ |> aliasFromDocs ]

        Elm.Docs.ValueBlock value ->
            Elm.apply (Elm.val "ValueBlock")
                [ value |> valueFromDocs ]

        Elm.Docs.BinopBlock binop ->
            Elm.apply (Elm.val "BinopBlock")
                [ binop |> binopFromDocs ]

        Elm.Docs.UnknownBlock string ->
            Elm.apply (Elm.val "UnknownBlock")
                [ string |> escape |> Elm.string ]
    )
        |> Elm.withType (Annotation.named [] "Block")
