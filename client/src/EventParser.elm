-- TODO: key value pairs are optional
-- TODO: parsing should continue even if one event failed to parse
-- TODO: parsing for days

module EventParser exposing (..)

import Parser exposing (..)
import List

type alias Key = String
type alias Value = String
type alias Id = String
type alias Descriptors = List (Key, Value)

type Event = Event Id Descriptors 
type alias Events = List Event

getEvents : String -> Result (List DeadEnd) Events
getEvents s = 
    run eventsParser s

eventsParser : Parser Events
eventsParser =
    loop [] eventsParserHelp

eventsParserHelp : Events -> Parser (Step (Events) (Events))
eventsParserHelp events = 
    oneOf
        [ succeed (\event -> Loop ( event :: events ))
            |. spaces
            |= eventParser
        , succeed ()
            |> map (\_ -> Done (List.reverse events))
        ]

eventParser : Parser Event
eventParser = 
    succeed (\id descriptors -> Event id descriptors)
    |= idParser
    |= descriptorsParser 

idParser : Parser Id
idParser =
    succeed (\id -> id)
    |. spaces
    |. symbol "["
    |. spaces
    |= keyParser
    |. spaces
    |. symbol "]"
    |. chompUntilEndOr "\n"

descriptorsParser : Parser Descriptors
descriptorsParser = 
    loop [] descriptorHelp

descriptorHelp : Descriptors -> Parser (Step (Descriptors) (Descriptors))
descriptorHelp pairs  = 
    oneOf
        [ succeed (\key value -> Loop ( (key, value) :: pairs ))
            |. spaces
            |= keyParser -- parsing the key
            |. spaces
            |. symbol "="
            |.spaces
            |= valueParser -- parsing the value
            |. chompUntilEndOr "\n"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse pairs))
        ]

keyParser : Parser String
keyParser = 
    getChompedString <|
        succeed ()
        |. chompWhile isValidKeyChar

valueParser : Parser String
valueParser = 
    getChompedString <|
        succeed ()
        |. chompWhile isValidValueChar
        
isValidKeyChar : Char -> Bool
isValidKeyChar char = 
    Char.isAlphaNum char || 
    char == '.' || 
    char == '#' || 
    char == '_'

isValidValueChar : Char -> Bool
isValidValueChar char = 
    char /= '=' && 
    char /= '[' && 
    char /= ']' && 
    char /= '\n'

