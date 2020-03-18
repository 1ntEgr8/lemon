module Calendar exposing (..)

import EventParser exposing (Events, Event)

type alias Start = Int
type alias End = Int
type RowRange = Row Start End 
type ColumnRange = Column Start End 
type Calendar  = List (Event, RowRange, ColumnRange)

getCalendar : Events -> Calendar
getCalendar = []

-- [ (3, 4)
-- , (3, 4)
-- , (3:15, 3:30)
-- , (3:30, 5:30)
-- , (4:30, 6:00)
-- ]

-- 1. determine the bounds
--      (3, 6)
-- 2. get precision
--      * number after the :
--      * divide by 60
-- 3. no of rows = (upper - lower) / precision
-- 4. row = (val.0 - lower) + 1
--    range = row + (val.1 - val.0)
-- 5. conflict resolution through columns
--      no of columns = no of consecutive overlaps
--      in a sorted list of time tuples
-- 6. populate the list with the last filled time
--      if there is no overlap, that's the column
--      otherwise, check the next one
--

