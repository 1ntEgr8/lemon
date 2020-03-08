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

-- Events = List Event
-- Event = Event Id Descriptors
type alias Model = Events

init : () -> (Model, Cmd Msg)
init _ = 
    ( [ Event "id" [(Custom "key", "val")] ]
    , Cmd.none
    )

type Msg = NewEditorInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewEditorInput s ->
            let res = getEvents s in
            case res of
                Ok events -> (events, Cmd.none)
                Err _ -> (model, Cmd.none)
                -- Err _ -> ([ Event "error" [(Custom "oh-no", "error")] ] , Cmd.none) 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model  =
    div []
    [ header
    , body model 
    , footer
    ]

header = 
    div [ class "header" ] [ text "lemon" ]

body model =
    div [ class "body" ] 
    [ outline model
    , cal 
    , editor
    ]

outline : Events -> Html Msg
outline events = 
    div [ classList
            [ ("container", True)
            , ("outline", True)
            ]
        ] (List.map eventContainer events)
        
 
cal = 
    div [ classList
            [ ("container", True)
            , ("calendar", True)
            ]
        ] []

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

parseText : String -> Msg
parseText s = NewEditorInput s
 
eventContainer : Event -> Html Msg
eventContainer (Event id descriptors) =
   div [ classList 
            [ ("event", True)
            ]
       ]
       [ h3 [] [ text id ]
       , div [] (List.map descriptorContainer descriptors)
       ]
        
descriptorContainer : Descriptor -> Html Msg
descriptorContainer (key, value) = 
    case key of
        Name -> div [ class "name" ] [ text value ]
        Time -> div [ class "time" ] [ text ("time " ++ value) ]
        Location -> div [ class "location" ] [ text ("location " ++ value) ]
        Custom s -> div [] [ text (s ++ " " ++ value) ]

