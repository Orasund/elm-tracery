module ElmBook.Gen.Module exposing (..)

{-| 
-}



modules : List { name : String, content : List Block }
modules =
    [ { name = "Tracery"
      , content =
          [ MarkdownBlock
              """ Tracery is a text-generation language mostly used for twitter bots.

See [Tracery.io](www.tracery.io) for more information.
"""
          , ValueBlock
              { name = "fromJson"
              , comment =
                  """ Turns a tracery json-string into a generator

    import Json.Decode
    import Random
    import Result.Extra

    generate : Int -> String -> String
    generate seed json =
        json
            |> Tracery.fromJson
            |> Result.Extra.unpack
                Json.Decode.errorToString
                (\\grammar ->
                    Random.step (Tracery.run grammar) (Random.initialSeed seed)
                        |> Tuple.first
                )

A tracery json is a object that has a `origin` field.

The `#` and `\\` characters need to be escaped.

    \"\"\"
    { \"origin\": \"The \\\\\\\\# and \\\\\\\\\\\\\\\\ characters need to be escaped.\"}
    \"\"\"
    |> generate 42
    --> \"The # and \\\\ characters need to be escaped.\"

If you provide a list, tracer will tick an element at random.

    \"\"\"
    { \"origin\": [\"I like cats\",\"I like dogs\"]}
    \"\"\"
    |> generate 42
    --> \"I like cats\"

You can reference other fields using `#..#`

    \"\"\"
    { \"origin\": [\"I have two pets: a #pet# and a #pet#\"]
    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]
    }
    \"\"\"
    |> generate 42
    --> \"I have two pets: a dog and a cat\"

You can also save partially evaluated strings

    \"\"\"
    { \"origin\": [\"I both have a #myPet# and a #myPet#.\"]
    , \"myPet\" : \"#petWithColor#\"
    , \"petWithColor\" : [\"black #pet#\", \"white #pet#\", \"brown #pet#\"]
    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]
    }
    \"\"\"
    |> generate 41
    --> \"I both have a white cat and a white parrot.\"

Definitions may also be recursive.

    \"\"\"
    { \"origin\": [\"I have #pets#\"]
    , \"pets\": [\"a #pet#\",\"a #pet# and #pets#\"]
    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]
    }
    \"\"\"
    |> generate 20
    --> \"I have a fish and a cat and a dog\"

You can define constants by providing a string instead of a list.

    \"\"\"
    { \"origin\": [\"My #favoritePet# is the best #favoritePet# in the world\"]
    , \"favoritePet\" : \"#pet#\"
    , \"pet\": [\"cat\",\"dog\",\"fish\",\"parrot\"]
    }
    \"\"\"
    |> generate 42
    --> \"My dog is the best dog in the world\"

You may define sub-definitions to organize your definitions.

    \"\"\"
    { \"origin\": [\"My #cat#\",\"My #dog#\"]
    , \"cat\":
      { \"origin\":\"cat is named #name#\"
      , \"name\": [\"Cleopatra\",\"Cesar\"]
      }
    , \"dog\":
      { \"origin\":\"dog is named #name#\"
      , \"name\": [\"Athena\",\"Zeus\"]
      }
    }
    \"\"\"
    |> generate 42
    --> \"My cat is named Cleopatra\"

"""
              , type_ =
                  Lambda
                      (Type "String.String" [])
                      (Type
                          "Result.Result"
                          [ Type "Json.Decode.Error" []
                          , Type "Tracery.Grammar.Grammar" []
                          ]
                      )
              }
          , ValueBlock
              { name = "run"
              , comment =
                  """ Runs a grammar until it ends.

Some recursive definitions might take a long time.

Use `runTo` if you want to avoid long waiting times.

"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Grammar" [])
                      (Type "Random.Generator" [ Type "String.String" [] ])
              }
          , ValueBlock
              { name = "runTo"
              , comment =
                  """ Runs a grammar until it reaches a key in the list.

    import Json.Decode
    import Random
    import Result.Extra

    generateTo : List String -> ({variable:String} -> String)-> Int -> String -> String
    generateTo list fun seed json =
        json
            |> Tracery.fromJson
            |> Result.Extra.unpack
                Json.Decode.errorToString
                (\\grammar ->
                    Random.step (Tracery.runTo list grammar) (Random.initialSeed seed)
                        |> Tuple.first
                        |> toString fun
                )

    \"\"\"
    { \"origin\": [\"A #color# #animal#\"]
    , \"color\": [\"black\",\"white\",\"gray\"]
    , \"animal\":
      [ \"cat, looking at a #color# #animal#\"
      , \"bird.\"
      ]
    }
    \"\"\"
    |> generateTo [\"animal\"] (\\{variable} -> \"dog.\") 42
    --> \"A black dog.\"

"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "String.String" [] ])
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type
                              "Random.Generator"
                              [ Type "Tracery.Grammar.Grammar" [] ]
                          )
                      )
              }
          , ValueBlock
              { name = "toString"
              , comment =
                  """ Will just write the current output.

use run or runTo, to actually compute something.

"""
              , type_ =
                  Lambda
                      (Lambda
                          (Record
                              [ ( "variable", Type "String.String" [] ) ]
                              Nothing
                          )
                          (Type "String.String" [])
                      )
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type "String.String" [])
                      )
              }
          ]
      }
    , { name = "Tracery.Command"
      , content =
          [ MarkdownBlock
              """ Commands are used to be able to pause the execution of a Grammar.

By modifying the commands in a grammar you can directly change how the program should run.
"""
          , UnionBlock
              { name = "Command"
              , comment =
                  """ Defines commands that the algorithm recognizes

  - Print - print the expression to the output
  - Define - Add a set of definitions
  - Delete - Delete a set of definitions
  - Save - saves the current value of the output as a constant and replaces it with a different value.

"""
              , args = []
              , tags =
                  [ ( "Print", [ Type "Tracery.Syntax.Expression" [] ] )
                  , ( "Define"
                    , [ Type
                          "Dict.Dict"
                          [ Type "String.String" []
                          , Type "Tracery.Syntax.Definition" []
                          ]
                      ]
                    )
                  , ( "Delete"
                    , [ Type "List.List" [ Type "String.String" [] ] ]
                    )
                  , ( "Save"
                    , [ Record
                          [ ( "asConstant", Type "String.String" [] )
                          , ( "replaceWith"
                            , Type
                                  "List.List"
                                  [ Type "Tracery.Command.Command" [] ]
                            )
                          ]
                          Nothing
                      ]
                    )
                  ]
              }
          , ValueBlock
              { name = "simplify"
              , comment = """ simplifies the commands.
"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
              }
          , ValueBlock
              { name = "toString"
              , comment =
                  """ Turns the list of commands into a readable string
"""
              , type_ =
                  Lambda
                      (Lambda
                          (Type "String.String" [])
                          (Type "String.String" [])
                      )
                      (Lambda
                          (Type
                              "List.List"
                              [ Type "Tracery.Command.Command" [] ]
                          )
                          (Type "String.String" [])
                      )
              }
          , ValueBlock
              { name = "fillAll"
              , comment = """ replaces all variables
"""
              , type_ =
                  Lambda
                      (Lambda
                          (Type "String.String" [])
                          (Type
                              "Random.Generator"
                              [ Type
                                  "List.List"
                                  [ Type "Tracery.Command.Command" [] ]
                              ]
                          )
                      )
                      (Lambda
                          (Type
                              "List.List"
                              [ Type "Tracery.Command.Command" [] ]
                          )
                          (Type
                              "Random.Generator"
                              [ Type
                                  "List.List"
                                  [ Type "Tracery.Command.Command" [] ]
                              ]
                          )
                      )
              }
          , ValueBlock
              { name = "fromExpressions"
              , comment = """ Convert expressions to commands
"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "Tracery.Syntax.Expression" [] ])
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
              }
          , ValueBlock
              { name = "variables"
              , comment = """ Returns all variables.
"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
                      (Type "List.List" [ Type "String.String" [] ])
              }
          , ValueBlock
              { name = "onlyValues"
              , comment =
                  """ States if only values are in the list. This essentially means, that all processing has been done.
"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
                      (Type "Basics.Bool" [])
              }
          ]
      }
    , { name = "Tracery.Grammar"
      , content =
          [ MarkdownBlock
              """ Creates a string generator based on a syntax.


# Grammar
"""
          , AliasBlock
              { name = "Grammar"
              , comment =
                  """

  - `next` - what needs to be generated next?
  - `constants` - what
  - `definitions` - the grammar rules (see Syntax)

"""
              , args = []
              , type_ =
                  Record
                      [ ( "output"
                        , Type "List.List" [ Type "Tracery.Command.Command" [] ]
                        )
                      , ( "stack"
                        , Type "List.List" [ Type "Tracery.Command.Command" [] ]
                        )
                      , ( "next"
                        , Type
                              "Maybe.Maybe"
                              [ Type "Tracery.Command.Command" [] ]
                        )
                      , ( "constants"
                        , Type
                              "Dict.Dict"
                              [ Type "String.String" []
                              , Type
                                  "List.List"
                                  [ Type "Tracery.Command.Command" [] ]
                              ]
                        )
                      , ( "definitions"
                        , Type
                              "Dict.Dict"
                              [ Type "String.String" []
                              , Type "Tracery.Syntax.Definition" []
                              ]
                        )
                      ]
                      Nothing
              }
          , ValueBlock
              { name = "fromDefinitions"
              , comment = """ Turns Definitions into a Grammar.
"""
              , type_ =
                  Lambda
                      (Type
                          "Dict.Dict"
                          [ Type "String.String" []
                          , Type "Tracery.Syntax.Definition" []
                          ]
                      )
                      (Type "Tracery.Grammar.Grammar" [])
              }
          , ValueBlock
              { name = "toString"
              , comment = """ Prints the Grammar
"""
              , type_ =
                  Lambda
                      (Lambda
                          (Record
                              [ ( "variable", Type "String.String" [] ) ]
                              Nothing
                          )
                          (Type "String.String" [])
                      )
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type "String.String" [])
                      )
              }
          , MarkdownBlock """


# Generating
"""
          , ValueBlock
              { name = "generateWhile"
              , comment =
                  """ Generates a string while a predicate is valid

    import Dict
    import Json.Decode
    import Random exposing (Generator)
    import Result.Extra
    import Tracery.Syntax exposing (Definition(..), Expression(..))
    import Set

    andThenToString : ({ variable : String } -> String) -> Int -> (Grammar -> Generator Grammar) -> Grammar -> String
    andThenToString fun seed gen grammar =
        Random.step (gen grammar) (Random.initialSeed seed)
            |> Tuple.first
            |> toString fun

    input : Grammar
    input =
        [ ( \"origin\", Choose [ [ Value \"A \", Variable \"animal\" ] ] )
        , ( \"animal\"
          , Choose
                [ [ Value \"cat, looking at a \", Variable \"animal\" ]
                , [ Value \"bird.\" ]
                ]
          )
        ]
            |> Dict.fromList
            |> fromDefinitions

    input
    |> andThenToString (\\{variable} -> \"dog.\") 42 (generateWhile (\\_ -> True) defaultStrategy)
    --> \"A cat, looking at a cat, looking at a cat, looking at a cat, looking at a bird.\"

    input
    |> andThenToString (\\{variable} -> \"dog.\") 42 (generateWhile (\\_ -> True) (noRecursionStrategy (Set.fromList [\"animal\"])))
    --> \"A bird.\"

"""
              , type_ =
                  Lambda
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type "Basics.Bool" [])
                      )
                      (Lambda
                          (Type "Tracery.Grammar.Strategy" [])
                          (Lambda
                              (Type "Tracery.Grammar.Grammar" [])
                              (Type
                                  "Random.Generator"
                                  [ Type "Tracery.Grammar.Grammar" [] ]
                              )
                          )
                      )
              }
          , ValueBlock
              { name = "generateOutput"
              , comment =
                  """ Generates an output found in the resulting grammar.

You can use `generateCommands` instead, If you intend to get the output right away.

"""
              , type_ =
                  Lambda
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type "Basics.Bool" [])
                      )
                      (Lambda
                          (Type "Tracery.Grammar.Strategy" [])
                          (Lambda
                              (Type "Tracery.Grammar.Grammar" [])
                              (Type
                                  "Random.Generator"
                                  [ Type "Tracery.Grammar.Grammar" [] ]
                              )
                          )
                      )
              }
          , ValueBlock
              { name = "generateNext"
              , comment =
                  """ Computes the command in `grammar.next`.

Afterwards the next command gets loaded

    import Dict
    import Json.Decode
    import Random exposing (Generator)
    import Result.Extra
    import Tracery.Syntax exposing (Definition(..), Expression(..))

    andThenToString : ({ variable : String } -> String) -> Int -> (Grammar -> Generator Grammar) -> Grammar -> String
    andThenToString fun seed gen grammar =
        Random.step (gen grammar) (Random.initialSeed seed)
            |> Tuple.first
            |> toString fun

    input : Grammar
    input =
        [ ( \"origin\", Choose [ [ Value \"A \", Variable \"animal\" ] ] )
        , ( \"animal\"
          , Choose
                [ [ Value \"cat, looking at a \", Variable \"animal\" ]
                , [ Value \"bird.\" ]
                ]
          )
        ]
            |> Dict.fromList
            |> fromDefinitions

using this function, you can step through the computation

    input
    |> andThenToString (\\{variable} -> \"<\" ++ variable ++ \">.\") 42 (generateNext defaultStrategy)
    --> \"A <animal>.\"

The second step does nothing (some steps only perform internal rearrangements)

    input
    |> andThenToString (\\{variable} -> \"<\" ++ variable ++ \">.\") 42
        (\\g -> g
            |> (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
        )
    --> \"A <animal>.\"

But after a few more steps (and some rewinding), we get the result

    input
    |> andThenToString (\\{variable} -> \"dog.\") 42
        (\\g -> g
            |> (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
            |> Random.andThen (generateNext defaultStrategy)
            |> Random.map rewind
            |> Random.andThen (generateNext defaultStrategy)
             |> Random.andThen (generateNext defaultStrategy)
        )
    --> \"A cat, looking at a dog.\"

"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Strategy" [])
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type
                              "Random.Generator"
                              [ Type "Tracery.Grammar.Grammar" [] ]
                          )
                      )
              }
          , MarkdownBlock """


# Strategy
"""
          , AliasBlock
              { name = "Strategy"
              , comment =
                  """ The strategy specifies the algorithm to choose an option
"""
              , args = []
              , type_ =
                  Lambda
                      (Type "String.String" [])
                      (Lambda
                          (Type
                              "List.List"
                              [ Type "Tracery.Syntax.Expression" [] ]
                          )
                          (Type "Basics.Bool" [])
                      )
              }
          , ValueBlock
              { name = "defaultStrategy"
              , comment = """ This strategy will choose any option
"""
              , type_ =
                  Lambda
                      (Type "String.String" [])
                      (Lambda
                          (Type
                              "List.List"
                              [ Type "Tracery.Syntax.Expression" [] ]
                          )
                          (Type "Basics.Bool" [])
                      )
              }
          , ValueBlock
              { name = "noRecursionStrategy"
              , comment =
                  """ This strategy will never choose a recursive option
"""
              , type_ =
                  Lambda
                      (Type "Set.Set" [ Type "String.String" [] ])
                      (Lambda
                          (Type "String.String" [])
                          (Lambda
                              (Type
                                  "List.List"
                                  [ Type "Tracery.Syntax.Expression" [] ]
                              )
                              (Type "Basics.Bool" [])
                          )
                      )
              }
          , ValueBlock
              { name = "onlyRecursionStrategy"
              , comment =
                  """ This strategy will only chose recursive options
"""
              , type_ =
                  Lambda
                      (Type "Set.Set" [ Type "String.String" [] ])
                      (Lambda
                          (Type "String.String" [])
                          (Lambda
                              (Type
                                  "List.List"
                                  [ Type "Tracery.Syntax.Expression" [] ]
                              )
                              (Type "Basics.Bool" [])
                          )
                      )
              }
          , MarkdownBlock """


# Technical Utilities
"""
          , ValueBlock
              { name = "toNext"
              , comment =
                  """ Moves to the next command without executing anything.

    toNext : Grammar -> Grammar
    toNext grammar =
        grammar |> withCommands grammar.stack

"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Grammar" [])
                      (Type "Tracery.Grammar.Grammar" [])
              }
          , ValueBlock
              { name = "withCommands"
              , comment = """ Sets Commands of a Grammar.
"""
              , type_ =
                  Lambda
                      (Type "List.List" [ Type "Tracery.Command.Command" [] ])
                      (Lambda
                          (Type "Tracery.Grammar.Grammar" [])
                          (Type "Tracery.Grammar.Grammar" [])
                      )
              }
          , ValueBlock
              { name = "skip"
              , comment =
                  """ Puts the current command on the output (without executing it) and then gets the next command.
"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Grammar" [])
                      (Type "Tracery.Grammar.Grammar" [])
              }
          , ValueBlock
              { name = "rewind"
              , comment = """ sets the output as input.
"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Grammar" [])
                      (Type "Tracery.Grammar.Grammar" [])
              }
          , ValueBlock
              { name = "end"
              , comment = """ set the remaining commands as output
"""
              , type_ =
                  Lambda
                      (Type "Tracery.Grammar.Grammar" [])
                      (Type "Tracery.Grammar.Grammar" [])
              }
          ]
      }
    , { name = "Tracery.Syntax"
      , content =
          [ MarkdownBlock
              """ This modules exposes the internal structures of the package.

Its intended to be used in combination with some preprocessing.
"""
          , UnionBlock
              { name = "Expression"
              , comment =
                  """ The expressions always return a string

  - `Value` - just return the given string
  - `Variable` - look up the key and insert a generated string according to the definition of the key.

"""
              , args = []
              , tags =
                  [ ( "Value", [ Type "String.String" [] ] )
                  , ( "Variable", [ Type "String.String" [] ] )
                  ]
              }
          , UnionBlock
              { name = "Definition"
              , comment =
                  """ The definition specifies how the strings gets generated

  - `Choose` - Choose a random sentence out of a list.
  - `Let` - Generate the sentence one. Then use the sentence over and over again.
  - `With` - Generate the sentence according to the sub-grammar.

"""
              , args = []
              , tags =
                  [ ( "Choose"
                    , [ Type
                          "List.List"
                          [ Type
                              "List.List"
                              [ Type "Tracery.Syntax.Expression" [] ]
                          ]
                      ]
                    )
                  , ( "Let"
                    , [ Type "List.List" [ Type "Tracery.Syntax.Expression" [] ]
                      ]
                    )
                  , ( "With"
                    , [ Type
                          "Dict.Dict"
                          [ Type "String.String" []
                          , Type "Tracery.Syntax.Definition" []
                          ]
                      ]
                    )
                  ]
              }
          , ValueBlock
              { name = "decoder"
              , comment = """ Decoder for the Syntax.
"""
              , type_ =
                  Type
                      "Json.Decode.Decoder"
                      [ Type
                          "Dict.Dict"
                          [ Type "String.String" []
                          , Type "Tracery.Syntax.Definition" []
                          ]
                      ]
              }
          , ValueBlock
              { name = "fromString"
              , comment =
                  """

    import Dict exposing (Dict)
    import Tracery.Command exposing (Command(..))

    input : String
    input =
       \"\"\"{ \"origin\" : [ \"Hello \\\\\\\\\\\\\\\\ World \\\\\\\\#\", \"#statement# and #statement#\" ]
       , \"statement\" :
         { \"origin\" : \"my #myPet# is the #complement#\"
         , \"myPet\": \"#pet#\"
         , \"pet\" : [\"cat\",\"dog\"]
         , \"complement\" : [\"smartest #myPet# in the world\",\"fastest #myPet# that i know of\"]
         }
       }\"\"\"

    output : Dict String Definition
    output =
        Dict.fromList
            [ ( \"origin\"
              , Choose
                  [ [ (Value \"Hello \"),  (Value \"\\\\\"),  (Value \" World \"), (Value \"#\")]
                  , [ (Variable \"statement\"),  (Value \" and \"),  (Variable \"statement\")]
                  ]
              )
            , ( \"statement\"
              , Dict.fromList
                  [ ( \"origin\"
                    , [  (Value \"my \")
                      ,  (Variable \"myPet\")
                      ,  (Value \" is the \")
                      ,  (Variable \"complement\")
                      ]
                        |> Let
                    )
                  , ( \"myPet\",Let [ (Variable \"pet\")])
                  , ( \"pet\", Choose [[ (Value \"cat\")],[ (Value \"dog\")]])
                  , ( \"complement\"
                    , Choose
                        [ [  (Value \"smartest \"),  (Variable \"myPet\"),  (Value \" in the world\")]
                        , [  (Value \"fastest \"),  (Variable \"myPet\"),  (Value \" that i know of\")]
                        ]
                    )
                  ]
                    |> With
                )
            ]

    input |> fromString
    --> Ok output

"""
              , type_ =
                  Lambda
                      (Type "String.String" [])
                      (Type
                          "Result.Result"
                          [ Type "Json.Decode.Error" []
                          , Type
                              "Dict.Dict"
                              [ Type "String.String" []
                              , Type "Tracery.Syntax.Definition" []
                              ]
                          ]
                      )
              }
          , ValueBlock
              { name = "originString"
              , comment =
                  """ The origin is the starting point for the generated story.

    originString : String
    originString =
        \"origin\"

"""
              , type_ = Type "String.String" []
              }
          ]
      }
    ]


{- --------------------------
   Types
-------------------------- -}


type Type
    = Var String
    | Lambda Type Type
    | Tuple (List Type)
    | Type String (List Type)
    | Record (List ( String, Type )) (Maybe String)


type Block
    = MarkdownBlock String
    | UnionBlock Union
    | AliasBlock Alias
    | ValueBlock Value
    | BinopBlock Binop
    | UnknownBlock String


type alias Union =
    { name : String
    , comment : String
    , args : List String
    , tags : List ( String, List Type )
    }


type alias Alias =
    { name : String, comment : String, args : List String, type_ : Type }


type alias Value =
    { name : String, comment : String, type_ : Type }


type Associativity
    = Left
    | None
    | Right


type alias Binop =
    { name : String
    , comment : String
    , type_ : Type
    , associativity : Associativity
    , precedence : Int
    }


