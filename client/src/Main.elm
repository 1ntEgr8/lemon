-- TODO: prev event state, and new event state
-- update the state only when the parser returns a valid value
-- otherwise render the old state
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

type Msg = ParseEditorText String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ParseEditorText s ->
            case (getEvents s) of
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
    [ outline
    , cal model 
    , editor
    ]

outline = 
    div [ classList
            [ ("container", True)
            , ("outline", True)
            ]
        ] []

cal : Events -> Html Msg
cal events = 
    div [ classList
            [ ("container", True)
            , ("calendar", True)
            ]
        ] [ div [] 
            ( events 
            |> List.map (\(Event id descriptors) ->  descriptorDivs descriptors) 
            |> List.map (\e -> div [] e) 
            )
          ]

editor = 
    div [ classList
            [ ("container", True)
            , ("editor", True)
            ]
        ] 
        [ textarea 
            [ class "editor-text-area"
            , spellcheck False
            , onInput (\s -> ParseEditorText s) 
            ] [] 
        ]

footer = 
    div [ class "footer" ] [ text "made with <3 by 1ntEgr8" ]

descriptorDivs : Descriptors -> List (Html Msg)
descriptorDivs descriptors =
    List.map (\(_, y) -> div [] [ text ("value: " ++ y) ] ) descriptors

