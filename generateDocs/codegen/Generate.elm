module Generate exposing (main)

{-| -}

import Docs
import Elm.Docs exposing (Module)
import Gen.CodeGen.Generate as Generate
import Json.Decode as D


main =
    Generate.fromJson (D.list Elm.Docs.decoder)
        (\modules ->
            [ Docs.fromModules modules ]
        )
