module Main exposing (init)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import CalendarParser exposing (..)
import List
import Dict exposing (Dict)


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
    , cal  model
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


cal : Calendar -> Html Msg
cal calendar = 
    div [ classList
            [ ("container", True)
            , ("calendar", True)
            ]
        , style 
            "grid-template-columns"
            (gridColumnsCalendar calendar)
        ] (generateCalendar calendar)


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
            ) (sortEvents events)
        )


getDescriptorsDiv : Descriptors -> Html Msg
getDescriptorsDiv descriptors =
    table 
        [ class "outline-level-3" ]
        (List.map (\(k, v) -> 
            tr
                []
                [ td [ class "key" ] [ text k ]
                , td [ class "value" ] [ text (toString v) ]
                ]
            ) (Dict.toList descriptors)
        )


toString : Value -> String
toString v =
    case v of
        TimeRange start end -> 
            String.fromFloat start ++ ", " ++ String.fromFloat end
        Custom s ->
            s

-- generate agenda for each day
generateCalendar : Calendar -> List (Html Msg)
generateCalendar calendar =
    List.map (\day -> 
        let
            times = filterTimes day
            (starts, ends) = List.unzip times
            ltime = 
                case List.minimum starts of
                    Just t -> t
                    Nothing -> -1
            htime = 
                case List.maximum ends of
                    Just t -> t
                    Nothing -> -1
            diffs = List.map (\(start, end) -> end - start) times
            minDiff = 
                case List.minimum diffs of
                    Just t -> t
                    Nothing -> -1
        in
            div
                [ class "day"
                , style
                    "grid-template-rows"
                    (gridRowDay ltime htime minDiff)
                ]
                (renderCal day ltime)
            ) calendar


gridColumnsCalendar : Calendar -> String
gridColumnsCalendar calendar =
    "repeat(" 
        ++ String.fromInt (List.length calendar) 
        ++ ", 100%)"


gridRowDay : Float -> Float -> Float -> String
gridRowDay ltime htime minDiff =
    "repeat("
        ++ String.fromFloat ((htime - ltime) / minDiff)
        ++ ", 20%)"

-- row = time - BASE + 1
renderCal : Day -> Float -> List (Html Msg)
renderCal day base = 
    List.map (\event ->
        let
            (start, end) = 
                case Dict.get "time" event.descriptors of
                    Just v -> getTimeFrom v
                    Nothing -> (-1, -1)
            rowStart = start - base + 1
            rowEnd = end - base + 1
        in
            div
                [ style "grid-row-start" (String.fromFloat rowStart)
                , style "grid-row-end" (String.fromFloat rowEnd)
                , style "background" "pink"
                ]
                [ text "test" ]
    ) day.events


sortEvents : (List Event) -> (List Event)
sortEvents events =
    List.sortWith (\e1 e2 ->
        let
            d1 = e1.descriptors
            d2 = e2.descriptors
        in
            case Dict.get "time" d1 of
                Just v1 ->
                    case Dict.get "time" d2 of
                        Just v2 ->
                            let
                                (st1, en1) = getTimeFrom v1
                                (st2, en2) = getTimeFrom v2
                            in
                                if st1 > st2 then
                                    GT
                                else if en1 > en2 then
                                    GT
                                else
                                    LT
                        Nothing ->
                            LT
                Nothing ->
                    GT
        ) events


filterTimes : Day -> List (Float, Float)
filterTimes day =
    List.map (\event ->
        case Dict.get "time" event.descriptors of
            Just v ->
                getTimeFrom v
            Nothing ->
                (-1, -1)
        ) day.events


getTimeFrom : Value -> (Start, End) 
getTimeFrom val =
    case val of
        TimeRange s e -> (s, e)
        _ -> (-1, -1)


