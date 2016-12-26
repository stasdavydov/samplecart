module CartEncoder exposing (cart)

import Cart2 exposing (Cart, Item, Product)
import List exposing (map)
import Json.Encode exposing (..)

product : Product -> Value
product product =
  object
    [ ("name", string product.name)
    , ("price", float product.price)
    ]

item : Item -> Value
item item =
  object
    [ ("product", product item.product)
    , ("qty", int item.qty)
    ]

cart : Cart -> Value
cart cart =
  list (map item cart)

