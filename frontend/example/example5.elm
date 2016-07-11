import List exposing (..)
import String exposing (toInt)
import Cart2 exposing (..)
import CartEncoder exposing (cart)
import CartDecoder
import Numeral exposing (format)
import Html exposing (Html, body, button, table, caption, thead, tbody, tfoot, tr, td, th, text, section, p, h1, input)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import WebSocket
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)

server : String
server =
  "ws://127.0.0.1:8765"


{-| Our stock is just a list of stuff. Lets think the stock has infinity stuff amount, no restrictions. -}
type alias Stock = List Stuff


{-| One of the key points -- the model of the app.
    It is a record with cart and stock, nothing more. -}
type alias Model =
  { cart : Cart
  , stock : Stock
  , error : Maybe String
  , player_carts : List Cart
  }


{-| The main function will be executed on the app start.
    It initializes the Elm program with model, view and update. -}
main =
  Html.program
    { init = init
    , view = view
    , update = updateOnServer
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)

init =
  let
    model = Model [] -- empty cart
      [ Stuff "Bicycle" 100.50 -- stock
      , Stuff "Rocket" 15.36
      , Stuff "Bisquit" 21.15
      ]
      Nothing -- error (no error at beginning)
      [] -- player carts list is empty
  in
    (!) model [ WebSocket.send server (encode 0 (cart model.cart)) ]


{-| We have only three messages: 1) adding a stuff into the cart; 2) change quantity of stuff in a cart;
    3) updating list of players carts from server -}
type Msg = Add Stuff | ChangeQty Stuff String | PlayerCarts String


updateOnServer : Msg -> Model -> (Model, Cmd Msg)

updateOnServer msg model =
  let
    (newModel, have_to_send) =
      update msg model
  in
    case have_to_send of
      True ->
        (!) newModel [ WebSocket.send server (encode 0 (cart newModel.cart)) ]

      False ->
        (!) newModel [Cmd.none]



{-| Definition of the controller function.
    It takes a message, model and return new moel based on the message handling.
    How we handle the Add message? Update the model's cart with new stuff come with the message. -}
update : Msg -> Model -> (Model, Bool)

update msg model =
  case msg of
    Add stuff ->
      ({ model | cart = add model.cart stuff }, True)

    ChangeQty stuff str ->
      case toInt str of
        Ok qty ->
          case changeQty model.cart stuff qty of
            Ok cart ->
              ({ model | cart = cart, error = Nothing }, True)

            Err msg ->
              ({ model | error = Just msg }, False)

        Err msg ->
          ({ model | error = Just msg }, False)

    PlayerCarts message ->
      case decodeString (Json.Decode.list CartDecoder.cart) message of
        Ok carts ->
          ({ model | player_carts = carts }, False)

        Err msg ->
          ({ model | error = Just msg, player_carts = [] }, False)



subscriptions : Model -> Sub Msg

subscriptions model =
  WebSocket.listen server PlayerCarts


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
    , cartView model.cart False
    , errorView model.error
    , playersCartsView model.player_carts
    ]


{-| Stock view works with the stock. It's a table of stuff we have in the stock.
    I placed some layout data like align and width right into the code to make
    the layout more usable. There are Elm libraries for better CSS style representation.
    So the stock view is a table with header and body of stuff rows.
    The stockStuffView is mapped to all the stock stuff. -}
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
    , tbody [] (map stockStuffView stock)
    ]


{-| The helper function for stuff row in the stock.
    Please look at the "Add to Cart" button. You see how the message Add Stuff is linked to the button onClick event. -}
stockStuffView : Stuff -> Html Msg

stockStuffView stuff =
  tr []
    [ td [] [ text stuff.name ]
    , td [align "right"] [ text (formatPrice stuff.price) ]
    , td [] [ button [ onClick (Add stuff) ] [ text "Add to Cart" ] ]
    ]

{-| Cart view is another table with cart items. This view doesn't send any messages yet but the
    function return type should be the same Html Msg. Elm validates all types during compilation.
    The cartSruffView function is mapped to all the cart items.
    The Cart is not just a stuff list with quanitites. It has a subtotal calculated based on the stuff in the cart. -}
cartView : Cart -> Bool -> Html Msg

cartView cart readonly =
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
      , tbody [] ( map (\stuff -> cartStuffView stuff readonly) cart )
      , tfoot []
        [ tr [style [("font-weight", "bold")]]
          [ td [ align "right", colspan 4 ] [ text ( formatPrice (subtotal cart)) ] ]
        ]
      ]

{-| Just a row in the cart table. -}
cartStuffView : Item -> Bool -> Html Msg

cartStuffView item readonly =
  tr []
    [ td [] [ text item.stuff.name ]
    , td [ align "right" ] [ text (formatPrice item.stuff.price) ]
    , td [ align "center" ]
      [ if readonly
          then text (toString item.qty)
          else input
            [ value (toString item.qty)
            , onInput (ChangeQty item.stuff)
            , size 3
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


playersCartsView : List Cart -> Html Msg

playersCartsView carts =
  if isEmpty carts
    then p [] [ text "Nobody plays yet." ]
    else section []
      [ h1 [] [ text "Players' Carts" ]
      , section []
        (map (\cart -> cartView cart True) carts)
      ]
