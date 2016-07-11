module Cart1 exposing
  ( Cart, Item, Stuff
  , add, subtotal
  , itemSubtotal
  ) -- This is module and its API definition

{-| We build an easy shopping cart.
@docs Cart, Item, Stuff, add, subtotal, itemSubtotal
-}

import List exposing (..) -- we need list manipulation functions


{-| Cart is a list of items. -}
type alias Cart = List Item

{-| Item is a record of stuff with quantity. -}
type alias Item = { stuff : Stuff, qty : Int }

{-| Stuff is a record with name and price -}
type alias Stuff = { name : String, price : Float }


{-| We want to add stuff to a cart.
    This is a function definition, it takes a cart, a stuff to add and returns new cart -}
add : Cart -> Stuff -> Cart

{-| This is an implementation of the 'add' function.
    Just append stuff item to the cart if there is no such stuff in the cart listed.
    Do nothing if the stuff exists. -}
add cart stuff =
  if isEmpty (filter (\item -> item.stuff == stuff) cart)
    then append cart [Item stuff 1]
    else cart


{-| I need to calculate cart subtotal.
    The function takes a cart and returns float. -}
subtotal : Cart -> Float

{-| It's easy -- just sum subtotal of all items. -}
subtotal cart = sum (map itemSubtotal cart)


{-| Item subtotal takes item and return the subtotal float. -}
itemSubtotal : Item -> Float

{-| Subtotal is a product of stuff's price and quantity. -}
itemSubtotal item = item.stuff.price * toFloat item.qty
