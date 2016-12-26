{-| We build an easy shopping cart. -}

import List exposing (..) -- we need list manipulation functions

cart = [] -- cart is a list of items

item product quantity = { product = product, qty = quantity } -- item is a record of product and quantity.

product name price = { name = name, price = price } -- product is a record of name and price

add cart product = -- we want to add product to a cart
  if isEmpty (filter (\item -> item.product == product) cart)
    then append cart [item product 1] -- append new item to the cart if there is no such product in the cart
    else cart -- or do nothing otherwise

subtotal cart = -- we want to calculate cart subtotal
  sum (map (\item -> item.product.price * toFloat item.qty) cart) -- sum qty * price of all items
