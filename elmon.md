# The elmon syntax

This is a description of the schedule description language used in lemon. I am writing this so that it is easier to reason about things when coding, but most importantly so that the parser is developed beautifully.

## Motivation

This is how I envision to use the app. It is not a calendar. It is rather a way to quickly plan out your day and then export a calendar if needed. More like a prototyping software, but for your planning your day :)

This system can also be extended to support planning events. This was my main motivation for making lemon. I had a very hard time hashing out a schedule in Google Sheets, and was very eager to just type out my thoughts and have the computer do the gui stuff for me. This is what lemon aims to do. To type out the schedule, I strongly felt the need for a very simple description language. Yes, there's JSON, YAML, TOML, ... but I felt that it would intimidate people without technical background. The aim with elmon is to make it so simple and intuitive to use that I just need to show you one example and you'll be ready to describe the most complex of all schedules!!!

## How to describe a schedule in elmon

A user starts off by specifying the day.
A day conists of a list of events. 
Events can be tagged.

## Sample elmon file

```
{ day 1 }

[ event1-name ]
# fun, hard, lol

time = 3-4pm
description = short description
custom = value

[ event2-name ]
# joe, mama

time = 3-4pm
description = short description
custom = value

{ day 2 }

[ event3-name ]
time = 3-4pm
description = short description
custom = value

[ event4-name ]
time = 3-4pm
description = short description
custom = value


```

## Syntax

```
{ <day> }
```
This is used to specify a day. The days can be specified in any order, but I expect users to type out their schedules in a chronological manner. 

`<day> = day Number | DayOfTheWeek | Date` 

```
[ <event name> ]
```
This is how you describe an event. Each event has key value pairs and optional tags. The tags can be used by a parser to classify your events. The key value pairs provide essential meta data (like time) which can be used by a graphics software to render the calendar.

`<event name> = Any sequence of UNICODE characters except [ and ]` 

```
key = value
```
This is how you specify a key-value pair. Some parsers can identify common key-value pairs. For example, lemon handles time, location and description keys in a special way. 
`key = [a-zA-Z0-9]`
The value can be anything!

```
# tag1, tag2
```
This is how you give tags to events. It must immediately follow the name of the event. The syntax is self-explanatory :)


