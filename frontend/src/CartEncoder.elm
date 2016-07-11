module CartEncoder exposing (cart)

import Cart2 exposing (..)
import List exposing (map)
import Json.Encode exposing (..)

stuff : Stuff -> Value
stuff stuff =
  object
    [ ("name", string stuff.name)
    , ("price", float stuff.price)
    ]

item : Item -> Value
item item =
  object
    [ ("stuff", stuff item.stuff)
    , ("qty", int item.qty)
    ]

cart : Cart -> Value
cart cart =
  list (map item cart)

