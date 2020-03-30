module Calendar exposing (..)

type alias Start = Int
type alias End = Int
type RowRange = Row Start End 
type ColumnRange = Column Start End 

{-
    how do i continue this?

    to render the calendar what do i need?

    Calendar = List Day
   
    - write a parser to get the time
        takes in a string
        returns Result (Start, End) Err
    - 


    step 1
        sort the data 
            for mvp, no need to sort by day
            to sort by event
                we do it based on the time field
                if time field doesn't exist
                    it is considered to by inf



-}
