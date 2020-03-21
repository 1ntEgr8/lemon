-- TODO multiple events associated to the same tag feature (implemented using --)
-- TODO add day feature
-- TODO figure out the calendar alg
-- TODO figure out the calendar css
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


-- Init
type alias Model
    = Events


init : () -> (Model, Cmd Msg)
init _ = 
    ( [ Event "tag" [(Custom "key", "val")] ]
    , Cmd.none
    )


-- Update
type Msg 
    = NewEditorInput String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewEditorInput s ->
            let res = getEvents s in
            case res of
                Ok events -> (events, Cmd.none)
                Err _ -> (model, Cmd.none)
                -- Err _ -> ([ Event "error" [(Custom "oh-no", "error")] ] , Cmd.none) 


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View
view : Model -> Html Msg
view model  =
    div []
    [ header
    , body model 
    , footer
    ]


-- View helper methods
header : Html Msg
header = 
    div [ class "header" ] [ text "lemon" ]


body : Model -> Html Msg
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
            , onInput parseText
            ] [] 
        ]


footer : Html Msg
footer = 
    div [ class "footer" ] [ text "made with <3 by 1ntEgr8" ]


parseText : String -> Msg
parseText s = 
    NewEditorInput s


eventContainer : Event -> Html Msg
eventContainer (Event tag descriptors) =
   div [ classList 
            [ ("event", True)
            ]
       ]
       [ h4 
            [ id tag 
            ]
            [ text tag 
            ]
       , div [] (List.map descriptorContainer descriptors)
       ]


descriptorContainer : Descriptor -> Html Msg
descriptorContainer (key, value) = 
    case key of
        Name -> div [ class "name" ] [ text value ]
        Time -> div [ class "time" ] [ text ("time " ++ value) ]
        Location -> div [ class "location" ] [ text ("location " ++ value) ]
        Custom s -> div [] [ text (s ++ " " ++ value) ]


