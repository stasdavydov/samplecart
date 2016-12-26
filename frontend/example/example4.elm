import List exposing (..)
import String exposing (toInt)
import Cart2 exposing (..)
import Numeral exposing (format)
import Html exposing (Html, body, button, table, caption, thead, tbody, tfoot, tr, td, th, text, section, p, h1, input)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)


{-| Our stock is just a list of product. Lets think the stock has infinity product amount, no restrictions. -}
type alias Stock = List Product


{-| One of the key points -- the model of the app.
    It is a record with cart and stock, nothing more. -}
type alias Model = { cart : Cart, stock : Stock, error : Maybe String }


{-| The main function will be executed on the app start.
    It initializes the Elm program with model, view and update. -}
main =
  Html.beginnerProgram
    { model = Model [] -- empty cart
      [ Product "Bicycle" 100.50 -- stock
      , Product "Rocket" 15.36
      , Product "Bisquit" 21.15
      ]
      Nothing -- error (no error at beginning)
    , view = view
    , update = update
    }

{-| We have only one message -- adding a product into the cart -}
type Msg = Add Product | ChangeQty Product String

{-| Defenition of the controller function.
    It takes a message, model and return new moel based on the message handling.
    How we handle the Add message? Update the model's cart with new product come with the message. -}
update : Msg -> Model -> Model

update msg model =
  case msg of
    Add product ->
      { model | cart = add model.cart product }

    ChangeQty product str ->
      case toInt str of
        Ok qty ->
          case changeQty model.cart product qty of
            Ok cart ->
              { model | cart = cart, error = Nothing }

            Err msg ->
              { model | error = Just msg }

        Err msg ->
          { model | error = Just msg }


{-| This is a view. Translation function of the model into HTML.
    It's not just a static HTML code, it can generate messages.
    As you see the view is built from HTML tag and attribute named functions like 'body' or 'style'.
    Every tag function (like 'body') get two arguments:
    - list of attributes;
    - list of wrapped tags.

    Lets split the UI into two parts: Stock and Cart. -}
view : Model -> Html Msg

view model =
  section [style [("margin", "10px")]]
    [ stockView model.stock
    , cartView model.cart
    , errorView model.error
    ]


{-| Stock view works with the stock. It's a table of product we have in the stock.
    I placed some layout data like align and width right into the code to make
    the layout more usable. There are Elm libraries for better CSS style representation.
    So the stock view is a table with header and body of product rows.
    The stockProductView is mapped to all the stock product. -}
stockView : Stock -> Html Msg

stockView stock =
  table []
    [ caption [] [ h1 [] [ text "Stock" ] ]
    , thead []
      [ tr []
        [ th [align "left", width 100] [ text "Name" ]
        , th [align "right", width 100] [ text "Price" ]
        , th [width 100] []
        ]
      ]
    , tbody [] (map stockProductView stock)
    ]


{-| The helper function for product row in the stock.
    Please look at the "Add to Cart" button. You see how the message Add Product is linked to the button onClick event. -}
stockProductView : Product -> Html Msg

stockProductView product =
  tr []
    [ td [] [ text product.name ]
    , td [align "right"] [ text (formatPrice product.price) ]
    , td [] [ button [ onClick (Add product) ] [ text "Add to Cart" ] ]
    ]

{-| Cart view is another table with cart items. This view doesn't send any messages yet but the
    function return type should be the same Html Msg. Elm validates all types during compilation.
    The cartSruffView function is mapped to all the cart items.
    The Cart is not just a product list with quanitites. It has a subtotal calculated based on the product in the cart. -}
cartView : Cart -> Html Msg

cartView cart =
  if isEmpty cart
    then p [] [ text "Cart is empty" ]
    else table []
      [ caption [] [ h1 [] [ text "Cart" ]]
      , thead []
        [ tr []
          [ th [ align "left", width 100 ] [ text "Name" ]
          , th [ align "right", width 100 ] [ text "Price" ]
          , th [ align "center", width 30 ] [ text "Qty" ]
          , th [ align "right", width 100 ] [ text "Subtotal" ]
          ]
        ]
      , tbody [] (map cartProductView cart)
      , tfoot []
        [ tr [style [("font-weight", "bold")]]
          [ td [ align "right", colspan 4 ] [ text ( formatPrice (subtotal cart)) ] ]
        ]
      ]

{-| Just a row in the cart table. -}
cartProductView : Item -> Html Msg

cartProductView item =
  tr []
    [ td [] [ text item.product.name ]
    , td [ align "right" ] [ text (formatPrice item.product.price) ]
    , td [ align "center" ]
      [ input
        [ value (toString item.qty)
        , onInput (ChangeQty item.product)
        , size 3
        --, type' "number"
        ] []
      ]
    , td [ align "right" ] [ text (formatPrice (itemSubtotal item)) ]
    ]


errorView : Maybe String -> Html Msg

errorView error =
  case error of
    Just msg ->
      p [style [("color", "red")]] [ text msg ]

    Nothing ->
      p [] []



formatPrice : Float -> String

formatPrice price =
  format "$0,0.00" price
