module Test1 exposing (main)

import Html
import Json.Decode
import Random
import Tracery


seed : Random.Seed
seed =
    Random.initialSeed 42


main =
    """
    { "origin": 
    ["#greeting##emoji# #funFact# #fact# #outro#"
    , "#fact# #funFact# #greeting# #emoji#"
    ]
, "greeting":
    [ "Hi #emoji##emoji#"
    , "I like pizza"
    , "What are you looking at? "
    , "single af"
    , "brain"
    , "Hi im awesome"
    , "Im new here"
    , "Im different"
    ]
, "number": ["1,20", "1,50", "2,0","2,4"," hard to get","#age#"]
, "age": ["16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","33","35","too old for you", "too young for you", "old enough #emoji#"]
, "funFact": 
    [ "love #love#"
    , "Im #number#" 
    , "#study# student"
    , "#funFact# #funFact#"
    ]
, "fact":
    [ "Have a Ph.D. in snuggling"
    , "Fun is my first name."
    , "fun at home, fun alone"
    , "it takes to two to bugalow"
    , "not interested in dating"
    , "im the one you're looking for"
    , "you'll be the pretty one"
    ,"moved here for work"
    , "Im exactly as I look"
    , "Way prettier in real life"
    , "My ex would recommend me"
    ]
, "love":
    [ "sushi 🍣"
    , "cats 😺\u{200B}"
    , "dogs 🐕"
    , "coffee"
    ]
, "study":
    [ "psychology", "science", "arts", "law" ]
, "outro":
    [ "random"
    , "no bad jokes pls"
    , "swipe right if youre #swipeRightIf#"
    , "10/10 would fuck again"
    , "pls be nice"
    , "One Love"
    , "like it rough"
    , "dont swipe right if youre #swipeRightIf#"
    ]
, "swipeRightIf":["ready for #readyFor#","ugly","my ex","into #love#"]
, "readyFor":["action #emoji#","my hips #emoji#","some love #emoji#","#emoji#"]
, "emoji":["👅","\u{200B}🔥\u{200B}","\u{200B}👀\u{200B}","\u{200B}👻\u{200B}","🤪","💪","❤"]
}
    """
        |> Tracery.fromJson
        |> (\result ->
                case result of
                    Err err ->
                        Json.Decode.errorToString err

                    Ok generator ->
                        Random.step generator seed
                            |> Tuple.first
           )
        |> Html.text
