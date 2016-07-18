import List exposing (..)
import String exposing (toInt)
import Cart3 exposing (..)
import CartEncoder
import CartDecoder
import Numeral exposing (format)
import Html exposing (Html, body, button, table, caption, thead, tbody, tfoot, tr, td, th, text, section, p, h1, h2, input, pre)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import WebSocket
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)

server : String
server =
  "ws://stasdavydov.com:8765"


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
  ( Model [] -- empty cart
    [ Stuff "Bicycle" 100.50 -- stock
    , Stuff "Rocket" 15.36
    , Stuff "Bisquit" 21.15
    ]
    Nothing -- error (no error at beginning)
    [] -- player carts list is empty
  , Cmd.none)


{-| We have only three messages: 1) adding a stuff into the cart; 2) change quantity of stuff in a cart;
    3) updating list of players carts from server -}
type Msg = Add Stuff | ChangeQty Stuff String | PlayerCarts String


{-| Update function wrapper. It will pass updated cart to server -}
updateOnServer : Msg -> Model -> (Model, Cmd Msg)

updateOnServer msg model =
  let
    (newModel, have_to_send) =
      update msg model
  in
    case have_to_send of
      True -> -- send updated cart to server
        (!) newModel [ WebSocket.send server (encode 0 (CartEncoder.cart newModel.cart)) ]

      False -> -- do nothing
        (newModel, Cmd.none)


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
    , cartView model.cart
    , errorView model.error
    , consumersCartsView model.player_carts
    ]


{-| Stock view works with the stock. It's a table of stuff we have in the stock.
    I placed some layout data like align and width right into the code to make
    the layout more usable. There are Elm libraries for better CSS style representation.
    So the stock view is a table with header and body of stuff rows.
    The stockStuffView is mapped to all the stock stuff. -}
stockView : Stock -> Html Msg

stockView stock =
  section [style [("background-color", "#FFC")]]
    [ h1 [] [ text "Stock" ]
    , table []
      [ thead []
        [ tr []
          [ th [align "left", width 100] [ text "Name" ]
          , th [align "right", width 100] [ text "Price" ]
          , th [width 100] []
          ]
        ]
      , tbody [] (map stockStuffView stock)
      ]
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
cartView : Cart -> Html Msg

cartView cart =
  section [style [("background-color", "#CFF")]]
    [ h1 [] [ text "Cart" ]
    , if isEmpty cart
        then p [] [ text "Add some stuff into cart" ]
        else table []
          [ thead []
            [ tr []
              [ th [ align "left", width 100 ] [ text "Name" ]
              , th [ align "right", width 100 ] [ text "Price" ]
              , th [ align "center", width 30 ] [ text "Qty" ]
              , th [ align "right", width 100 ] [ text "Subtotal" ]
              ]
            ]
          , tbody [] ( map (\stuff -> cartStuffView stuff) cart )
          , tfoot []
            [ tr [style [("font-weight", "bold")]]
              [ td [ align "right", colspan 4 ] [ text ( formatPrice (subtotal cart)) ] ]
            ]
          ]
    ]

{-| Just a row in the cart table. -}
cartStuffView : Item -> Html Msg

cartStuffView item =
  tr []
    [ td [] [ text item.stuff.name ]
    , td [ align "right" ] [ text (formatPrice item.stuff.price) ]
    , td [ align "center" ]
      [ input
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


consumersCartsView : List Cart -> Html Msg

consumersCartsView carts =
  if isEmpty carts
    then h1 [] [ text "Nobody is shopping yet" ]
    else
      section [style [("background-color", "#CFC")]]
      [ h1 [] [ text "Somebody is shopping" ]
      , table []
        [ thead []
          [ tr []
            [ th [align "left", width 50] [ text "#" ]
            , th [] [ text "Name" ]
            , th [align "right", width 100] [ text "Total Qty" ]
            , th [align "right", width 100] [ text "Subtotal" ]
            ]
          ]
        , tbody [] (indexedMap cartSummaryView carts)
        ]
      ]


{-| View for indexed cart summary -}
cartSummaryView : Int -> Cart -> Html Msg

cartSummaryView idx cart =
  if isEmpty cart
    then
      tr []
        [ td [] [ text (toString (idx+1)) ]
        , td [colspan 3] [ text "Empty user cart" ]
        ]
    else
      tr []
        [ td [] [ text (toString (idx+1)) ]
        , td [] [ pre [] (map (\item -> text (item.stuff.name ++ " (" ++ toString (item.qty) ++ ") " ++ "\n")) cart)]
        , td [align "right"] [ text (toString (qty cart)) ]
        , td [align "right"] [ text (formatPrice (subtotal cart)) ]
        ]
