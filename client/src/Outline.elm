-- this module handles everything related to the outline section
-- should define functions that take in a list of events, and return 
-- the html for the outline section in main
-- therefore, only one function needs to be exposed and everything 
-- else can be hidden

module Outline exposing (..)
import EventParser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List

outline : Events -> Html Msg
outline events = 
    div [ classList
            [ ("container", True)
            , ("outline", True)
            ]
        ] List.map eventContainer events
   
eventContainer : Event -> Html Msg
eventContainer (id, descriptors) =
   div [ classList 
            [ ("event", True)
            ]
       ]
       [ h3 [] [ text id ]
       , List.map descriptorContainer descriptors 
       ]
        
descriptorContainer : Descriptor -> Html Msg
descriptorContainer (key, value) = 
    case key of
        Name -> div [ class "name" ] [ text value ]
        Time -> div [ class "time" ] [ text ("time " ++ value) ]
        Location -> div [ class "location" ] [ text ("location " ++ location) ]
        Custom s -> div [] [ text (s ++ " " ++ value) ]

