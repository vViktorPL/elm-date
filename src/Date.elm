module Date exposing
    ( Date, fromYMD
    , fromIso8601, toIso8601, encode, decoder
    , Month, getMonth, getMonthYear, monthNumber, nextMonth, prevMonth, firstDayOfMonth, lastDayOfMonth, daysInMonth
    , Weekday(..), weekday
    , nextDay, prevDay, range, dayNumber
    )

{-|


# Create

@docs Date, fromYMD


# ISO8601

@docs fromIso8601, toIso8601, encode, decoder


# Months

@docs Month, getMonth, getMonthYear, monthNumber, nextMonth, prevMonth, firstDayOfMonth, lastDayOfMonth, daysInMonth


# Weeks

@docs Weekday, weekday


# Days

@docs nextDay, prevDay, range, dayNumber

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D exposing (fromMaybe)
import Json.Encode as E exposing (Value)
import Loop exposing (while)
import Task exposing (Task)


{-| Safe opaque type representing specific day
-}
type Date
    = Date Int Int Int


{-| Day of a week
-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| Specific month of a specific year
-}
type Month
    = Month Int Int


monthLength : Int -> Int -> Maybe Int
monthLength year month =
    case ( isLeapYear year, month ) of
        ( _, 1 ) ->
            Just 31

        ( True, 2 ) ->
            Just 29

        ( False, 2 ) ->
            Just 28

        ( _, 3 ) ->
            Just 31

        ( _, 4 ) ->
            Just 30

        ( _, 5 ) ->
            Just 31

        ( _, 6 ) ->
            Just 30

        ( _, 7 ) ->
            Just 31

        ( _, 8 ) ->
            Just 31

        ( _, 9 ) ->
            Just 30

        ( _, 10 ) ->
            Just 31

        ( _, 11 ) ->
            Just 30

        ( _, 12 ) ->
            Just 31

        _ ->
            Nothing


{-| Get number of days for specific Month

    let
        febIn2019 = getMonth (fromYMD 2019 2 1)
    in
        daysInMonth febIn2019 --> 28

-}
daysInMonth : Month -> Int
daysInMonth (Month year month) =
    -- monthLength won't return Nothing because
    -- Month is an opaque type that guarantees
    -- valid month, however we unpack a Maybe
    -- with some dummy "30" value as fallback
    -- for invalid month
    Maybe.withDefault 30 (monthLength year month)


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || (modBy 100 year /= 0 && modBy 4 year == 0)


{-| Get weekday for specific date

    weekday (fromYMD 2019 6 10) --> Mon

-}
weekday : Date -> Weekday
weekday (Date year month day) =
    let
        m =
            case month of
                1 ->
                    0

                2 ->
                    3

                3 ->
                    2

                4 ->
                    5

                5 ->
                    0

                6 ->
                    3

                7 ->
                    5

                8 ->
                    1

                9 ->
                    4

                10 ->
                    6

                11 ->
                    2

                _ ->
                    4

        y =
            if month < 3 then
                year - 1

            else
                year

        d =
            modBy 7 (y + y // 4 - y // 100 + y // 400 + m + day)
    in
    case d of
        0 ->
            Sun

        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        _ ->
            Sat


{-| Build date from year, month and day integers.
Month and day out of bounds values will be clamped.

    dayNumber (fromYMD 2019 6 10) --> 10

    -- Invalid date components are clamped to correct ones
    dayNumber (fromYMD 2019 6 31) --> 30

    (fromYMD 2019 13 1)
        |> getMonth
        |> monthNumber
    --> 12

-}
fromYMD : Int -> Int -> Int -> Date
fromYMD year month day =
    let
        safeMonthNumber =
            clamp 1 12 month

        safeMonth =
            Month year safeMonthNumber

        safeDayNumber =
            clamp 1 (daysInMonth safeMonth) day
    in
    Date year safeMonthNumber safeDayNumber


{-| Create date from ISO8601 date format string

    "2019-06-10"
        |> fromIso8601
        |> Maybe.map dayNumber
    --> Just 10

-}
fromIso8601 : String -> Maybe Date
fromIso8601 string =
    let
        components =
            string
                |> String.split "-"
                |> List.map String.toInt
    in
    case components of
        [ Just year, Just month, Just day ] ->
            if validateDateComponents ( year, month, day ) then
                Just (Date year month day)

            else
                Nothing

        _ ->
            Nothing


validateDateComponents : ( Int, Int, Int ) -> Bool
validateDateComponents ( year, month, day ) =
    monthLength year month
        |> Maybe.map (\maxDay -> day >= 1 && day <= maxDay)
        |> Maybe.withDefault False


twoDigitsFormat =
    String.padLeft 2 '0'


{-| Stringifies date to ISO8601 format string.

     toIso8601 (fromYMD 2019 1 31) --> "2019-01-31"

-}
toIso8601 : Date -> String
toIso8601 (Date year month day) =
    String.join "-"
        [ String.fromInt year
        , twoDigitsFormat (String.fromInt month)
        , twoDigitsFormat (String.fromInt day)
        ]


{-| Extract month from specific date.

Note: Month type beside month number consists also of year hence:

    getMonth (fromYMD 2019 6 10) == getMonth (fromYMD 2019 6 11) --> True

    getMonth (fromYMD 2019 6 10) == getMonth (fromYMD 2020 6 10) --> False

-}
getMonth : Date -> Month
getMonth (Date year month _) =
    Month year month


{-| Convert month into integer (1 for January, 12 for December)
-}
monthNumber : Month -> Int
monthNumber (Month _ month) =
    month


{-| Gets month next to provided one. If input month is December,
January of next year is returned.
-}
nextMonth : Month -> Month
nextMonth (Month year month) =
    case month of
        12 ->
            Month (year + 1) 1

        _ ->
            Month year (month + 1)


{-| Gets month previous to provided one. If input month is January,
December of previous year is returned.
-}
prevMonth : Month -> Month
prevMonth (Month year month) =
    case month of
        1 ->
            Month (year - 1) 12

        _ ->
            Month year (month - 1)


{-| Creates date for first day of a month.

    fromYMD 2019 12 31
        |> getMonth
        |> firstDayOfMonth
    --> fromYMD 2019 12 1

-}
firstDayOfMonth : Month -> Date
firstDayOfMonth (Month year month) =
    Date year month 1


{-| Creates date for last day of a month.

    fromYMD 2019 12 1
        |> getMonth
        |> lastDayOfMonth
    --> fromYMD 2019 12 31

-}
lastDayOfMonth : Month -> Date
lastDayOfMonth month =
    case month of
        Month yearInt monthInt ->
            Date yearInt monthInt (daysInMonth month)


{-| Adds one day to specific date.

    let
        date = fromYMD 2019 12 31
    in
        nextDay date --> fromYMD 2020 1 1

-}
nextDay : Date -> Date
nextDay (Date yearInt monthInt day) =
    let
        month =
            Month yearInt monthInt
    in
    if day == daysInMonth month then
        month |> nextMonth |> firstDayOfMonth

    else
        Date yearInt monthInt (day + 1)


{-| Subtracts one day from specific date.

    let
        date = fromYMD 2020 1 1
    in
        prevDay date --> fromYMD 2019 12 31

-}
prevDay : Date -> Date
prevDay (Date yearInt monthInt day) =
    let
        month =
            Month yearInt monthInt
    in
    if day == 1 then
        month |> prevMonth |> lastDayOfMonth

    else
        Date yearInt monthInt (day - 1)


isGreater : Date -> Date -> Bool
isGreater (Date ly lm ld) (Date ry rm rd) =
    ly
        > ry
        || (ly == ry)
        && (lm > rm)
        || (ly == ry)
        && (lm == rm)
        && (ld > rd)


{-| Creates list of dates between two specific dates.

    let
        startDate = fromYMD 2019 12 1
        endDate = fromYMD 2019 12 3
    in
        range startDate endDate
    --> [ fromYMD 2019 12 1
    --> , fromYMD 2019 12 2
    --> , fromYMD 2019 12 3
    --> ]

-}
range : Date -> Date -> List Date
range start end =
    let
        ( minDate, maxDate ) =
            if isGreater start end then
                ( end, start )

            else
                ( start, end )

        dateInBounds =
            (/=) minDate
    in
    Tuple.first
        (while
            (Tuple.second >> dateInBounds)
            (\( dates, lastDate ) ->
                let
                    currentDate =
                        prevDay lastDate
                in
                ( currentDate :: dates
                , currentDate
                )
            )
            ( [ maxDate ], maxDate )
        )


{-| Extracts day number (day of month) from date.

    dayNumber (fromYMD 2019 1 21) --> 21

-}
dayNumber : Date -> Int
dayNumber (Date _ _ day) =
    day


{-| Extracts year number from Month

    let
        jan2019 = getMonth (fromYMD 2019 1 1)
    in
        getMonthYear jan2019 --> 2019

-}
getMonthYear : Month -> Int
getMonthYear (Month year _) =
    year


{-| ISO8601 string to Date decoder
-}
decoder : Decoder Date
decoder =
    D.string
        |> D.map fromIso8601
        |> D.andThen (D.fromMaybe "Invalid date string")


{-| encode Date to ISO8601 string
-}
encode : Date -> Value
encode =
    toIso8601 >> E.string
