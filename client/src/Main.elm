module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import EventParser exposing (..)
import List

main = 
    Browser.element { init = init
                    , update = update
                    , view = view
                    , subscriptions = subscriptions
                    }

type alias Model = Event

init : () -> (Model, Cmd Msg)
init _ = 
    ( Event "id" [("key", "val")]
    , Cmd.none
    )

type Msg = Hello String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hello s ->
            let res = runEventParser s in
            case res of
                Ok event -> (event, Cmd.none)
                Err _ -> (Event "error" [("key", "error")] , Cmd.none) 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view (Event id descriptors) =
    div []
    [ header
    , body descriptors 
    , footer
    ]

header = 
    div [ class "header" ] [ text "lemon" ]

body descriptors =
    div [ class "body" ] 
    [ outline
    , cal descriptors
    , editor
    ]

outline = 
    div [ classList
            [ ("container", True)
            , ("outline", True)
            ]
        ] []

cal descriptors = 
    div [ classList
            [ ("container", True)
            , ("calendar", True)
            ]
        ] (calItems descriptors) 

editor = 
    div [ classList
            [ ("container", True)
            , ("editor", True)
            ]
        ] 
        [ textarea 
            [ class "editor-text-area"
            , spellcheck False
            , onInput parseText
            ] [] 
        ]

footer = 
    div [ class "footer" ] [ text "made with <3 by 1ntEgr8" ]

calItems : Descriptors -> List (Html Msg)
calItems descriptors =
    List.map (\(x, y) -> div [] [ text ("id: " ++ x ++ " value: " ++ y) ] ) descriptors

parseText : String -> Msg
parseText s = Hello s 
