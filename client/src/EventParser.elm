-- TODO: key value pairs are optional
-- TODO: parsing for days

module EventParser exposing (..)

import Parser exposing (..)
import List

type Key = Name 
    | Time 
    | Location 
    | Custom String
type alias Value = String
type alias Tag = String
type alias Descriptor = (Key, Value)
type alias Descriptors = List Descriptor

type Event = Event Tag Descriptors 
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
    succeed (\tag descriptors -> Event tag descriptors)
    |= tagParser 
    |= descriptorsParser 

tagParser : Parser Tag 
tagParser =
    succeed identity
    |. spaces
    |. symbol "["
    |. spaces
    |= tagValueParser 
    |. spaces
    |. symbol "]"
    |. spaces

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
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse pairs))
        ]

tagValueParser : Parser Tag 
tagValueParser =
    getChompedString <|
        succeed () 
        |. chompWhile isValidKeyChar

keyParser : Parser Key 
keyParser = 
    oneOf
        [ succeed Name
            |. keyword "name"
        , succeed Time
            |. keyword "time"
        , succeed Location
            |. keyword "location"
        , succeed (\s -> Custom s)
            |= tagValueParser
        ]

valueParser : Parser String
valueParser = 
    getChompedString <|
        succeed ()
        |. chompWhile isValidValueChar
        
isValidKeyChar : Char -> Bool
isValidKeyChar char = 
    Char.isAlphaNum char 
        || char == '.' 
        || char == '#' 
        || char == '_'
        || char == '-'

isValidValueChar : Char -> Bool
isValidValueChar char = 
    char /= '=' 
        && char /= '[' 
        && char /= ']' 
        && char /= '\n'

