module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
    Browser.sandbox { init = init, update = update, view = view }

-- my model is an array -- an array of what?
-- an array of events
-- each event has
--      description (optional)
--      time (not optional)
--      other info
-- you can assign your schedule to days later

type alias Model = Int

init : Model
init = 0

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Increment ->
            model + 1
        Decrement ->
            model - 1

view : Model -> Html Msg
view model = 
    div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

