{-| We build an easy shopping cart. -}

import List exposing (..) -- we need list manipulation functions

cart = [] -- cart is a list of items

item stuff quantity = { stuff = stuff, qty = quantity } -- item is a record of stuff and quantity.

stuff name price = { name = name, price = price } -- stuff is a record of name and price

add cart stuff = -- we want to add stuff to a cart
  if isEmpty (filter (\item -> item.stuff == stuff) cart)
    then append cart [item stuff 1] -- append new item to the cart if there is no such stuff in the cart
    else cart -- or do nothing otherwise

subtotal cart = -- we want to calculate cart subtotal
  sum (map (\item -> item.stuff.price * toFloat item.qty) cart) -- sum qty * price of all items
