module CalendarParser exposing (..)
import Parser exposing (..)
import Dict exposing (Dict)
import List


type alias Key = String


type alias Value = String


type alias Tags
    = List String


type alias Descriptors 
    = Dict Key Value 


type alias Name = String


type alias Event = 
    { name : String
    , tags : List String
    , descriptors : Descriptors
    }


-- modify how this gets parsed after the first run works
type alias DayName = String


type alias Day = 
    { value : String
    , events : List Event
    }


type alias Calendar
    = List Day


getCalendarFrom : String -> Result (List DeadEnd) Calendar
getCalendarFrom inputString = 
    run calendarParser inputString


calendarParser : Parser Calendar
calendarParser =
    loop [] calendarParserHelp 


calendarParserHelp : Calendar -> Parser (Step (List Day) (List Day))
calendarParserHelp calendar =
    oneOf
        [ succeed (\day -> Loop (day :: calendar))
            |. spaces
            |= dayParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse calendar))
        ]


dayParser : Parser Day
dayParser =
    succeed (\dayName events -> { value = dayName , events = events})
        |. spaces
        |. symbol "{"
        |. spaces
        |= valueParser
        |. spaces
        |. symbol "}"
        |. spaces
        |= eventsParser
        |. spaces


eventsParser : Parser (List Event) 
eventsParser =
    loop [] eventsParserHelp


eventsParserHelp : (List Event) -> Parser (Step (List Event) (List Event))
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
    succeed (\name descriptors -> { name = name, tags = [], descriptors = descriptors })
        |= nameParser
        |= descriptorsParser


nameParser : Parser Name
nameParser =
    succeed (\name -> name)
        |. spaces
        |. symbol "["
        |. spaces
        |= nameValueParser
        |. spaces
        |. symbol "]"
        |. spaces


tagsParser : Parser Tags
tagsParser =
    succeed (\tags -> tags)
        |. spaces
        |. symbol "#"
        |. spaces
        |= loop [] tagsParserHelp 
        |. spaces


tagsParserHelp : Tags -> Parser (Step (Tags) (Tags))
tagsParserHelp tags =
    oneOf
        [ succeed (\tag -> Loop (tag :: tags))
            |= tagParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse tags))
        ]


descriptorsParser : Parser Descriptors
descriptorsParser =
    loop Dict.empty descriptorsParserHelp


descriptorsParserHelp : Descriptors -> Parser (Step (Descriptors) (Descriptors))
descriptorsParserHelp dict =
    oneOf
        [ succeed (\k v -> Loop (Dict.insert k v dict))
            |. spaces
            |= keyParser
            |. spaces
            |. symbol "="
            |. spaces
            |= valueParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done dict)
        ]


nameValueParser : Parser Name
nameValueParser =
    getChompedString <|
        succeed ()
        |. chompWhile isValidValueChar


tagParser : Parser String
tagParser =
    getChompedString <|
        succeed ()
        |. chompWhile isValidKeyChar


keyParser : Parser Key
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
    Char.isAlphaNum char
        || char == '.'
        || char == '_'
        || char == '-'


isValidValueChar : Char -> Bool
isValidValueChar char =
    char /= '='
        && char /= '['
        && char /= ']'
        && char /= '{'
        && char /= '}'
        && char /= '\n'

