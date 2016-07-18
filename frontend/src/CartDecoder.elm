module CartDecoder exposing (cart)

import Cart2 exposing (Cart, Item, Stuff) -- decoding for Cart
import Json.Decode exposing (..) -- will decode cart from string

cart : Decoder (Cart)
cart =
  list item -- decoder for cart is a list of items

item : Decoder (Item)
item =
  object2 Item -- decoder for item is an object with two peoperties:
    ("stuff" := stuff) -- 1) "stuff" of stuff
    ("qty" := int) -- 2) "qty" of int

stuff : Decoder (Stuff)
stuff =
  object2 Stuff -- decoder for stuff also an object with two properties:
    ("name" := string) -- 1) "name"
    ("price" := float) -- 2) "price"
