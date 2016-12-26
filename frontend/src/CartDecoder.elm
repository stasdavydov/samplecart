module CartDecoder exposing (cart)

import Cart2 exposing (Cart, Item, Product) -- decoding for Cart
import Json.Decode exposing (..) -- will decode cart from string

cart : Decoder (Cart)
cart =
  list item -- decoder for cart is a list of items

item : Decoder (Item)
item =
  object2 Item -- decoder for item is an object with two peoperties:
    ("product" := product) -- 1) "product" of product
    ("qty" := int) -- 2) "qty" of int

product : Decoder (Product)
product =
  object2 Product -- decoder for product also an object with two properties:
    ("name" := string) -- 1) "name"
    ("price" := float) -- 2) "price"
