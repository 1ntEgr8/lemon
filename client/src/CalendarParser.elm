module CalendarParser exposing (..)
import Parser exposing (..)
import Dict exposing (Dict)
import List


type alias Key = String


type alias Start = Float 


type alias End = Float 


type Value 
    = TimeRange Start End 
    | Custom String


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


-- not needed for now
-- needs to be rewritten
-- parser goes into a loop when this is plugged in
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
        [ succeed (\k v -> Loop (addDescriptor k v dict))
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


addDescriptor : Key -> String -> Descriptors -> Descriptors
addDescriptor k v dict =
    case k of
        "time" -> 
            let
                res = parseTime v
            in
                case res of
                    Ok (start, end) -> 
                        Dict.insert k (TimeRange start end) dict
                    Err _ ->
                        dict
                    -- if there is an error in parsing time, ignore that entry
        _ ->
            Dict.insert k (Custom v) dict


-- TODO add an error type
parseTime : String -> Result (List DeadEnd) (Start, End)
parseTime timeString =
    run timeRangeParser timeString


-- TODO
--      check if the time is valid or not
--      get rid of parsing of floats
--  eg: 
--      8-9:30pm
--      9:30-8:30pm
timeRangeParser : Parser (Start, End) 
timeRangeParser =
    succeed (\start s_mod end e_mod-> modifyTimeRange (start, end) (s_mod, e_mod)) 
        |. spaces
        |= timeParser 
        |. spaces
        |= oneOf
            [ map (\_ -> "am") (keyword "am")
            , map (\_ -> "pm") (keyword "pm")
            , map (\_ -> "") (keyword "")
            ]
        |. spaces
        |. symbol "-"
        |. spaces
        |= timeParser
        |. spaces
        |= oneOf
            [ map (\_ -> "am") (keyword "am")
            , map (\_ -> "pm") (keyword "pm")
            , map (\_ -> "") (keyword "")
            ]
       

-- TODO 
-- check if the numbers passed in are valid
-- fix bug
--      10 - 12pm -> 10, 12; not 22, 24
timeParser : Parser Float
timeParser =
    succeed getTime 
        |= int
        |= oneOf 
            [ succeed identity
                |. symbol ":"
                |= int
            , succeed 0
            ]

modifyTimeRange : (Start, End) -> (String, String) -> (Start, End)
modifyTimeRange (start, end) modifiers  =
    case modifiers of
        ("", "pm") -> 
            (start + 12, end + 12)
        ("am", "pm") ->
            (start, end + 12)
        _ -> 
            (start, end)


getTime : Int -> Int -> Float
getTime hr minutes =
    toFloat hr + minutesToFraction minutes


minutesToFraction : Int -> Float
minutesToFraction minutes = 
    let
        res = String.toFloat ("0." ++ String.fromInt minutes)
    in
        case res of
            Just val ->
                toFloat (truncate (val / 0.6 * 100.0)) * 0.01
            Nothing  ->
                0



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

