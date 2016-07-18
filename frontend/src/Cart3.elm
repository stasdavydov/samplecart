module Cart3 exposing
  ( Cart, Item, Stuff
  , add, subtotal, qty
  , itemSubtotal, changeQty
  ) -- This is module and its API defenition

{-| We build an easy shopping cart.
@docs Cart, Item, Stuff, add, subtotal, itemSubtotal, changeQty, qty
-}

import List exposing (..) -- We need list functions.


{-| Cart is a list of items. -}
type alias Cart = List Item

{-| Item is a record of some stuff with quantity. -}
type alias Item = { stuff : Stuff, qty : Int }

{-| A stuff in a cart is a record with name and price for beginning -}
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


{-| We want to calculate cart subtotal.
    The function takes a cart and returns float. -}
subtotal : Cart -> Float

{-| It's easy -- just sum subtotal of all items. -}
subtotal cart = sum (map itemSubtotal cart)


{-| Calculate stuff quantity in the cart -}
qty : Cart -> Int

qty cart =
  foldl (+) 0 (map .qty cart)


{-| Item subtotal takes item and return the subtotal float. -}
itemSubtotal : Item -> Float

{-| Subtotal is a product of stuff's price and quantity. -}
itemSubtotal item = item.stuff.price * toFloat item.qty


{-| Change quantity of the stuff in the cart.
    Look at the result of the function. It uses Result type.
    The Result type has two parameters: for bad and for good result.
    So the result will be Error "msg" or a Cart with updated stuff quantity. -}
changeQty : Cart -> Stuff -> Int -> Result String Cart

{-| If the quantity parameter is zero the stuff will be removed completely from the cart.
    If the quantity parameter is greater then zero the quantity of the stuff will be updated.
    Otherwise (qty < 0) the error will be returned.
-}
changeQty cart stuff qty =
  if qty == 0 then
    Ok (filter (\item -> item.stuff /= stuff) cart)

  else if qty > 0 then
    Result.Ok (map (\item -> if item.stuff == stuff then { item | qty = qty } else item) cart)

  else
    Result.Err ("Wrong negative quantuty used: " ++ (toString qty))
