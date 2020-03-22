module Main exposing (init)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CalendarParser exposing (..)
import List
import Dict exposing (Dict)
import Parser


main = 
    Browser.element 
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- Init
type alias Model
    = Calendar 


init : () -> (Model, Cmd Msg)
init _ = 
    ( [Day "" []]
    , Cmd.none
    )


-- Update
type Msg 
    = Display Calendar 
    | ParseError String 


-- error calendar 
-- [ Day "error" [] ]
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Display calendar ->
            (calendar, Cmd.none)
        ParseError _ ->
            (model, Cmd.none)


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View
view : Model -> Html Msg
view model  =
    div []
    [ header
    , menubar
    , body model 
    , footer
    ]


-- View helper methods
header : Html Msg
header = 
    div [ class "header" ] [ text "lemon" ]


menubar : Html Msg
menubar = 
    div [ class "menubar" ] 
    [ div 
        [ contenteditable True ]
        [ text "Untitled"]
    ]


body : Model -> Html Msg
body model =
    div [ class "body" ] 
    [ outline model
    , cal 
    , editor
    ]

-- calendar = List Day = [ { value, events }, { value, events } ]
outline : Calendar -> Html Msg
outline calendar = 
    div [ classList
            [ ("container", True)
            , ("outline", True)
            ]
        ] (generateOutline calendar)


cal : Html Msg
cal = 
    div [ classList
            [ ("container", True)
            , ("calendar", True)
            ]
        ] []


editor : Html Msg
editor = 
    div [ classList
            [ ("container", True)
            , ("editor", True)
            ]
        ] 
        [ textarea 
            [ class "editor-text-area"
            , spellcheck False
            , onInput parseEditorText
            ] [] 
        ]


footer : Html Msg
footer = 
    div [ class "footer" ] [ text "made with <3 by 1ntEgr8" ]


parseEditorText : String -> Msg
parseEditorText editorText = 
    let 
        res = getCalendarFrom editorText 
    in
        case res of
            Ok calendar ->
                Display calendar 
            Err _ ->
                ParseError "error"

generateOutline : Calendar -> List (Html Msg)
generateOutline =
    List.map (\day -> 
        details 
            [ attribute "open" "true" ]
            [ summary [ class "outline-level-1" ] [ text day.value ] 
            , getEventsDiv day.events 
            ]
        )


getEventsDiv : List Event -> Html Msg
getEventsDiv events =
    div
        []
        (List.map (\event ->
            details
                []
                [ summary [ class "outline-level-2" ] [ text event.name ]
                , getDescriptorsDiv event.descriptors
                ]
            ) events
        )


getDescriptorsDiv : Descriptors -> Html Msg
getDescriptorsDiv descriptors =
    table 
        [ class "outline-level-3" ]
        (List.map (\(k, v) -> 
            tr
                []
                [ td [ class "key" ] [ text k ]
                , td [ class "value" ] [ text v ]
                ]
            ) (Dict.toList descriptors)
        )

