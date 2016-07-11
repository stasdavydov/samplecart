module CartDecoder exposing (cart)


import Cart2 exposing (..)
import Json.Decode exposing (..) -- will decode cart from string


cart : Decoder (Cart)
cart =
  list item


item : Decoder (Item)
item =
  object2 Item
    ("stuff" := stuff)
    ("qty" := int)

stuff : Decoder (Stuff)
stuff =
  object2 Stuff
    ("name" := string)
    ("price" := float)
