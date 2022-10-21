module Generate exposing (main)

{-| -}

import Docs
import Elm
import Elm.Annotation as Type
import Elm.Docs as Docs exposing (Module)
import Gen.CodeGen.Generate as Generate
import Gen.Helper
import Json.Decode as D


main =
    Generate.fromJson (D.list Docs.decoder)
        (\modules ->
            [ Docs.fromModules modules ]
        )
