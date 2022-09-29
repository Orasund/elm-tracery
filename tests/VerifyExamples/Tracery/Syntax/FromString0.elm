module VerifyExamples.Tracery.Syntax.FromString0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Tracery.Syntax exposing (..)
import Tracery.Trace exposing (Command(..))
import Dict



output : Syntax
output =
    Dict.fromList
        [ ( "origin"
          , Choose
              [ [ (Value "Hello "),  (Value "\\"),  (Value " World "), (Value "#")]
              , [ (Variable "statement"),  (Value " and "),  (Variable "statement")]
              ]
          )
        , ( "statement"
          , Dict.fromList
              [ ( "origin"
                , [  (Value "my ")
                  ,  (Variable "myPet")
                  ,  (Value " is the ")
                  ,  (Variable "complement")
                  ]
                    |> Let
                )
              , ( "myPet",Let [ (Variable "pet")])
              , ( "pet", Choose [[ (Value "cat")],[ (Value "dog")]])
              , ( "complement"
                , Choose
                    [ [  (Value "smartest "),  (Variable "myPet"),  (Value " in the world")]
                    , [  (Value "fastest "),  (Variable "myPet"),  (Value " that i know of")]
                    ]
                )
              ]
                |> With
            )
        ]
input : String
input =
   """{ "origin" : [ "Hello \\\\\\\\ World \\\\#", "#statement# and #statement#" ]
   , "statement" :
     { "origin" : "my #myPet# is the #complement#"
     , "myPet": "#pet#"
     , "pet" : ["cat","dog"]
     , "complement" : ["smartest #myPet# in the world","fastest #myPet# that i know of"]
     }
   }"""



spec0 : Test.Test
spec0 =
    Test.test "#fromString: \n\n    input |> fromString\n    --> Ok output" <|
        \() ->
            Expect.equal
                (
                input |> fromString
                )
                (
                Ok output
                )