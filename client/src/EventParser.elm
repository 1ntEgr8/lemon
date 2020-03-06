module EventParser exposing (..)

import Parser exposing (..)
import List

type alias Key = String
type alias Value = String
type alias Id = String
type alias Descriptors = List (Key, Value)

type Event = Event Id Descriptors 
type alias Events = List Event

runEventParser : String -> Result (List DeadEnd) Event
runEventParser s = 
    run eventParser s

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
        |. chompWhile (\c -> c /= '\n' && c /= '=')

isValidKeyChar : Char -> Bool
isValidKeyChar char = 
    Char.isAlphaNum char || char == '.' || char == '#' || char == '_'

