# elm-tracery

Interpreter for the Tracery language.

See [Tracery.io](www.tracery.io) for more information.

## Installation

```
elm install Orasund/elm-tracery
```

## How to use

```elm
import Tracery
import Random
import Json.Decode

seed : Random.Seed
seed =
    Random.initialSeed 41

"""
{ "origin":"I have a #favoriteAnimal# named #favoriteAnimalName# and a #animal# named #name#. I love #favoriteAnimalName# the most. It's the best #favoriteAnimal# in the world."
, "favoriteAnimal" : 
  { "origin":"#color# #animal#"
  , "color": ["white","black","brown"]
  }
, "favoriteAnimalName" : "#name#"
, "animal":["cat","dog","parrot"]
, "name": ["Johnny","Charlie","Twinkle","Charles"]
}
"""
|> Tracery.fromJson
|> (\result ->
    case result of
        Err err -> Json.Decode.errorToString err
        Ok generator ->
            Random.step generator seed
            |> Tuple.first
    )
--> "I have a black dog named Johnny and a parrot named Charles. I love Johnny the most. It's the best black dog in the world."
```

## Differences to the original Tracery

* No support of "saving" values. Instead values can be constant.
* Entry point is always `"origin"`.
* Supporting sub-grammars in favor of the more experimental features of the original.