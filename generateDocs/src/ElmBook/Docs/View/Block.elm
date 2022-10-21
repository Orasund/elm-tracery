module ElmBook.Docs.View.Block exposing (view)

{-| Original Code taken from <https://github.com/elm/package.elm-lang.org/blob/master/src/frontend/Page/Docs/Block.elm>
-}

import Dict
import ElmBook.Gen.Module as Docs
import Html exposing (..)
import Html.Attributes exposing (..)



-- CONSTANTS


maxWidth : Int
maxWidth =
    64



-- VIEW


view : Docs.Block -> String
view block =
    case block of
        Docs.MarkdownBlock markdown ->
            markdown

        Docs.ValueBlock value ->
            viewValue () value

        Docs.BinopBlock binop ->
            viewBinop () binop

        Docs.AliasBlock alias_ ->
            viewAlias () alias_

        Docs.UnionBlock union ->
            viewUnion () union

        Docs.UnknownBlock name ->
            "It seems that `"
                ++ name
                ++ "` does not have any docs. Please open a bug report "
                ++ "[here](TODO)"
                ++ " with the title “UnknownBlock found in docs” and with a link to this page in the description."


viewCodeBlock : String -> String -> List String -> String
viewCodeBlock name comment header =
    ("### " ++ name ++ "\n")
        ++ "```\n"
        ++ (header
                |> String.join "\n"
           )
        ++ "\n"
        ++ "```\n"
        ++ "\n"
        ++ comment



-- VIEW VALUE BLOCK


viewValue : Info -> Docs.Value -> String
viewValue info { name, comment, type_ } =
    viewCodeBlock name comment <|
        case toLines info Other type_ of
            One _ line ->
                [ name ++ space ++ colon ++ space ++ line ]

            More x xs ->
                (name ++ space ++ colon)
                    :: indentFour x
                    :: (xs |> List.map indentFour)


indentFour : String -> String
indentFour =
    (++) "    "



-- VIEW BINOP BLOCK


viewBinop : Info -> Docs.Binop -> String
viewBinop info { name, comment, type_ } =
    let
        nameHtml =
            "(" ++ name ++ ")"
    in
    viewCodeBlock name comment <|
        case toLines info Other type_ of
            One _ line ->
                [ nameHtml ++ space ++ colon ++ space ++ line ]

            More x xs ->
                (nameHtml ++ space ++ colon) :: indentFour x :: List.map indentFour xs



-- VIEW ALIAS BLOCK


viewAlias : Info -> Docs.Alias -> String
viewAlias info { name, args, comment, type_ } =
    let
        varsString =
            String.concat (List.map ((++) " ") args)

        aliasNameLine =
            "type"
                ++ space
                ++ "alias"
                ++ space
                ++ name
                ++ varsString
                ++ space
                ++ equals
    in
    viewCodeBlock name comment <|
        aliasNameLine
            :: List.map indentFour (linesToList (toLines info Other type_))



-- VIEW UNION


viewUnion : Info -> Docs.Union -> String
viewUnion info { name, comment, args, tags } =
    let
        varsString =
            String.concat <| List.map ((++) " ") args

        nameLine =
            "type" ++ space ++ name ++ varsString
    in
    viewCodeBlock name comment <|
        case tags of
            [] ->
                [ nameLine ]

            t :: ts ->
                nameLine :: linesToList (toMoreLines (unionMore info) t ts)


unionMore : Info -> MoreSettings ( String, List Docs.Type )
unionMore info =
    let
        ctorToLines ( ctor, args ) =
            toOneOrMore (toLines info Other (Docs.Type ctor args))
    in
    { open = "    = "
    , sep = "    | "
    , close = Nothing
    , openIndent = 6
    , sepIndent = 6
    , toLines = ctorToLines
    }



-- INFO


type alias Info =
    ()


type alias TypeNameDict =
    Dict.Dict String ( String, String )



-- CREATE LINKS


toLinkLine : Info -> String -> Lines String
toLinkLine info qualifiedName =
    let
        shortName =
            last qualifiedName (String.split "." qualifiedName)
    in
    One (String.length shortName) shortName


last : a -> List a -> a
last backup list =
    case list of
        [] ->
            backup

        x :: [] ->
            x

        _ :: xs ->
            last backup xs



-- LINES


type Lines line
    = One Int line
    | More line (List line)


type Context
    = Func
    | App
    | Other


toLines : Info -> Context -> Docs.Type -> Lines String
toLines info context tipe =
    case tipe of
        Docs.Var x ->
            One (String.length x) x

        Docs.Lambda arg result ->
            let
                lambdaToLine =
                    if context == Other then
                        toLinesHelp lambdaOne lambdaMore

                    else
                        toLinesHelp lambdaOneParens lambdaMoreParens
            in
            lambdaToLine (toLines info Func arg) <|
                List.map (toLines info Func) (collectArgs [] result)

        Docs.Tuple [] ->
            One 2 "()"

        Docs.Tuple (arg :: args) ->
            toLinesHelp tupleOne
                tupleMore
                (toLines info Other arg)
                (List.map (toLines info Other) args)

        Docs.Type name args ->
            let
                needsParens =
                    context == App && not (List.isEmpty args)
            in
            toLinesHelp
                (typeOne needsParens)
                (typeMore needsParens)
                (toLinkLine info name)
                (List.map (toLines info App) args)

        Docs.Record [] Nothing ->
            One 2 "{}"

        Docs.Record [] (Just ext) ->
            One (6 + String.length ext) ("{ " ++ ext ++ " | }")

        Docs.Record (f :: fs) extension ->
            let
                toLns ( field, fieldType ) =
                    ( field, toLines info Other fieldType )
            in
            case extension of
                Nothing ->
                    if List.isEmpty fs then
                        toLinesHelp recordOne recordMore (toLns f) (List.map toLns fs)

                    else
                        toMoreLines recordMore (toLns f) (List.map toLns fs)

                Just ext ->
                    case toLinesHelp (recordOneExt ext) recordMoreExt (toLns f) (List.map toLns fs) of
                        One width line ->
                            One width line

                        More first rest ->
                            More ("{ " ++ ext) (first :: rest ++ [ "}" ])



-- FUNCTIONS


collectArgs : List Docs.Type -> Docs.Type -> List Docs.Type
collectArgs revArgs tipe =
    case tipe of
        Docs.Lambda arg result ->
            collectArgs (arg :: revArgs) result

        _ ->
            List.reverse (tipe :: revArgs)


lambdaOne : OneSettings (Lines String)
lambdaOne =
    { open = ""
    , sep = " -> "
    , close = ""
    , openWidth = 0
    , sepWidth = 2
    , closeWidth = 0
    , toLine = toLine
    }


lambdaMore : MoreSettings (Lines String)
lambdaMore =
    { open = ""
    , sep = "-> "
    , close = Nothing
    , openIndent = 0
    , sepIndent = 3
    , toLines = toOneOrMore
    }


lambdaOneParens : OneSettings (Lines String)
lambdaOneParens =
    { open = "("
    , sep = " -> "
    , close = ")"
    , openWidth = 1
    , sepWidth = 2
    , closeWidth = 1
    , toLine = toLine
    }


lambdaMoreParens : MoreSettings (Lines String)
lambdaMoreParens =
    { open = "( "
    , sep = "  -> "
    , close = Just ")"
    , openIndent = 2
    , sepIndent = 5
    , toLines = toOneOrMore
    }



-- TUPLES


tupleOne : OneSettings (Lines String)
tupleOne =
    { open = "( "
    , sep = ", "
    , close = " )"
    , openWidth = 2
    , sepWidth = 2
    , closeWidth = 2
    , toLine = toLine
    }


tupleMore : MoreSettings (Lines String)
tupleMore =
    { open = "( "
    , sep = ", "
    , close = Just ")"
    , openIndent = 2
    , sepIndent = 2
    , toLines = toOneOrMore
    }



-- TYPES


typeOne : Bool -> OneSettings (Lines String)
typeOne needsParens =
    if needsParens then
        { open = "("
        , sep = " "
        , close = ")"
        , openWidth = 1
        , sepWidth = 1
        , closeWidth = 1
        , toLine = toLine
        }

    else
        { open = ""
        , sep = " "
        , close = ""
        , openWidth = 0
        , sepWidth = 1
        , closeWidth = 0
        , toLine = toLine
        }


typeMore : Bool -> MoreSettings (Lines String)
typeMore needsParens =
    if needsParens then
        { open = "("
        , sep = "    "
        , close = Just ")"
        , openIndent = 0
        , sepIndent = 4
        , toLines = toOneOrMore
        }

    else
        { open = ""
        , sep = "    "
        , close = Nothing
        , openIndent = 0
        , sepIndent = 4
        , toLines = toOneOrMore
        }



-- RECORDS


recordOne : OneSettings ( String, Lines String )
recordOne =
    { open = "{ "
    , sep = ", "
    , close = " }"
    , openWidth = 2
    , sepWidth = 2
    , closeWidth = 2
    , toLine = fieldToLine
    }


recordMore : MoreSettings ( String, Lines String )
recordMore =
    { open = "{ "
    , sep = ", "
    , close = Just "}"
    , openIndent = 6
    , sepIndent = 6
    , toLines = fieldToLines
    }



-- EXTENDED RECORDS


recordOneExt : String -> OneSettings ( String, Lines String )
recordOneExt extension =
    let
        open =
            "{ " ++ extension ++ " | "
    in
    { open = open
    , sep = ", "
    , close = " }"
    , openWidth = String.length open
    , sepWidth = 2
    , closeWidth = 2
    , toLine = fieldToLine
    }


recordMoreExt : MoreSettings ( String, Lines String )
recordMoreExt =
    { open = "    | "
    , sep = "    , "
    , close = Nothing
    , openIndent = 10
    , sepIndent = 10
    , toLines = fieldToLines
    }



-- RECORD HELPERS


fieldToLine : ( String, Lines String ) -> Maybe ( Int, String )
fieldToLine ( field, lines ) =
    case lines of
        More _ _ ->
            Nothing

        One width line ->
            Just ( String.length field + 3 + width, field ++ space ++ colon ++ space ++ line )


fieldToLines : ( String, Lines String ) -> OneOrMore String
fieldToLines ( field, lines ) =
    case lines of
        One width line ->
            let
                potentialWidth =
                    String.length field + 3 + width
            in
            if potentialWidth < maxWidth then
                OneOrMore (field ++ space ++ colon ++ space ++ line) []

            else
                OneOrMore (field ++ space ++ colon) [ line ]

        More x xs ->
            OneOrMore (field ++ space ++ colon) (x :: xs)



-- HELPERS


toLine : Lines line -> Maybe ( Int, line )
toLine lines =
    case lines of
        One width line ->
            Just ( width, line )

        More _ _ ->
            Nothing


linesToList : Lines line -> List line
linesToList lines =
    case lines of
        One _ line ->
            [ line ]

        More x xs ->
            x :: xs


type OneOrMore a
    = OneOrMore a (List a)


toOneOrMore : Lines line -> OneOrMore line
toOneOrMore lines =
    case lines of
        One _ line ->
            OneOrMore line []

        More x xs ->
            OneOrMore x xs



-- TO LINES HELP


type alias OneSettings a =
    { open : String
    , sep : String
    , close : String
    , openWidth : Int
    , sepWidth : Int
    , closeWidth : Int
    , toLine : a -> Maybe ( Int, String )
    }


type alias MoreSettings a =
    { open : String
    , sep : String
    , close : Maybe String
    , openIndent : Int
    , sepIndent : Int
    , toLines : a -> OneOrMore String
    }


toLinesHelp : OneSettings a -> MoreSettings a -> a -> List a -> Lines String
toLinesHelp one more x xs =
    let
        maybeOneLine =
            toOneLine one.openWidth one.open one (x :: xs)
    in
    case maybeOneLine of
        Just ( width, line ) ->
            One width line

        Nothing ->
            toMoreLines more x xs


toOneLine : Int -> String -> OneSettings a -> List a -> Maybe ( Int, String )
toOneLine chunkWidth chunk one entries =
    case entries of
        [] ->
            Just ( one.closeWidth, one.close )

        entry :: remainingEntries ->
            case one.toLine entry of
                Nothing ->
                    Nothing

                Just ( entryWidth, line ) ->
                    case toOneLine one.sepWidth one.sep one remainingEntries of
                        Nothing ->
                            Nothing

                        Just ( remainingWidth, remainingLine ) ->
                            let
                                width =
                                    chunkWidth + entryWidth + remainingWidth
                            in
                            if width < maxWidth then
                                Just ( width, chunk ++ line ++ remainingLine )

                            else
                                Nothing


toMoreLines : MoreSettings a -> a -> List a -> Lines String
toMoreLines s x xs =
    let
        (OneOrMore firstLine firstRest) =
            s.toLines x

        openIndentation =
            String.repeat s.openIndent " "

        sepIndentation : String
        sepIndentation =
            String.repeat s.sepIndent " "

        toChunk : OneOrMore String -> List String
        toChunk (OneOrMore y ys) =
            (s.sep ++ y) :: List.map ((++) sepIndentation) ys

        otherLines : List String
        otherLines =
            List.map ((++) openIndentation) firstRest
                ++ List.concatMap (s.toLines >> toChunk) xs
    in
    (case s.close of
        Nothing ->
            otherLines

        Just closer ->
            otherLines ++ [ closer ]
    )
        |> More (s.open ++ firstLine)



-- HELPERS


keyword : String -> Html msg
keyword kw =
    span [ class "hljs-keyword" ] [ text kw ]


space : String
space =
    " "


arrow : String
arrow =
    "->"


colon : String
colon =
    ":"


equals : String
equals =
    "="
