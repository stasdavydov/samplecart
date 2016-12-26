module Cart1 exposing
  ( Cart, Item, Product
  , add, subtotal
  , itemSubtotal
  ) -- This is module and its API definition

{-| We build an easy shopping cart.
@docs Cart, Item, Product, add, subtotal, itemSubtotal
-}

import List exposing (..) -- we need list manipulation functions


{-| Cart is a list of items. -}
type alias Cart = List Item

{-| Item is a record of product with quantity. -}
type alias Item = { product : Product, qty : Int }

{-| Product is a record with name and price -}
type alias Product = { name : String, price : Float }


{-| We want to add product to a cart.
    This is a function definition, it takes a cart, a product to add and returns new cart -}
add : Cart -> Product -> Cart

{-| This is an implementation of the 'add' function.
    Just append product item to the cart if there is no such product in the cart listed.
    Do nothing if the product exists. -}
add cart product =
  if isEmpty (filter (\item -> item.product == product) cart)
    then append cart [Item product 1]
    else cart


{-| I need to calculate cart subtotal.
    The function takes a cart and returns float. -}
subtotal : Cart -> Float

{-| It's easy -- just sum subtotal of all items. -}
subtotal cart = sum (map itemSubtotal cart)


{-| Item subtotal takes item and return the subtotal float. -}
itemSubtotal : Item -> Float

{-| Subtotal is a product of product's price and quantity. -}
itemSubtotal item = item.product.price * toFloat item.qty
