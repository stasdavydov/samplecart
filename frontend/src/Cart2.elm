module Cart2 exposing
  ( Cart, Item, Product
  , add, subtotal
  , itemSubtotal, changeQty
  ) -- This is module and its API defenition

{-| We build an easy shopping cart.
@docs Cart, Item, Product, add, subtotal, itemSubtotal, changeQty
-}

import List exposing (..) -- We need list functions.


{-| Cart is a list of items. -}
type alias Cart = List Item

{-| Item is a record of some product with quantity. -}
type alias Item = { product : Product, qty : Int }

{-| A product in a cart is a record with name and price for beginning -}
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


{-| We want to calculate cart subtotal.
    The function takes a cart and returns float. -}
subtotal : Cart -> Float

{-| It's easy -- just sum subtotal of all items. -}
subtotal cart = sum (map itemSubtotal cart)


{-| Item subtotal takes item and return the subtotal float. -}
itemSubtotal : Item -> Float

{-| Subtotal is a product of product's price and quantity. -}
itemSubtotal item = item.product.price * toFloat item.qty


{-| Change quantity of the product in the cart.
    Look at the result of the function. It uses Result type.
    The Result type has two parameters: for bad and for good result.
    So the result will be Error "msg" or a Cart with updated product quantity. -}
changeQty : Cart -> Product -> Int -> Result String Cart

{-| If the quantity parameter is zero the product will be removed completely from the cart.
    If the quantity parameter is greater then zero the quantity of the product will be updated.
    Otherwise (qty < 0) the error will be returned.
-}
changeQty cart product qty =
  if qty == 0 then
    Ok (filter (\item -> item.product /= product) cart)

  else if qty > 0 then
    Result.Ok (map (\item -> if item.product == product then { item | qty = qty } else item) cart)

  else
    Result.Err ("Wrong negative quantuty used: " ++ (toString qty))
