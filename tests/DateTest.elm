module DateTest exposing (suite)

import Date exposing (Date)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random exposing (maxInt)
import Test exposing (..)


fuzzDateComponents =
    Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 1 12, Fuzz.intRange 1 28 )


fuzzDate : Fuzzer Date
fuzzDate =
    Fuzz.map (\( year, month, day ) -> Date.fromYMD year month day) fuzzDateComponents


expectJust : Maybe a -> Expectation
expectJust maybe =
    case maybe of
        Just _ ->
            Expect.pass

        Nothing ->
            Expect.fail "Just expected, but got Nothing"


getMonthNumberFromDate =
    Date.getMonth >> Date.monthNumber


getYearNumberFromDate =
    Date.getMonth >> Date.getMonthYear


suite : Test
suite =
    describe "Date module"
        [ describe "Date.fromYMD"
            [ fuzz fuzzDateComponents "creates date from valid date components" <|
                \( year, month, day ) ->
                    Expect.all
                        [ \date -> Expect.equal day (Date.dayNumber date)
                        , \date -> Expect.equal month (getMonthNumberFromDate date)
                        , \date -> Expect.equal year (getYearNumberFromDate date)
                        ]
                        (Date.fromYMD year month day)
            , test "clamps zero month to January" <|
                \_ ->
                    Expect.equal 1 (getMonthNumberFromDate (Date.fromYMD 2019 0 1))
            , test "clamps 13 month to December" <|
                \_ ->
                    Expect.equal 12 (getMonthNumberFromDate (Date.fromYMD 2019 13 1))
            ]
        , describe "Date.fromIso8601"
            [ fuzz fuzzDateComponents "creates Just Date for valid ISO8601 strings" <|
                \( year, month, day ) ->
                    [ if year < 0 then
                        -year

                      else
                        year
                    , month
                    , day
                    ]
                        |> List.map String.fromInt
                        |> String.join "-"
                        |> Date.fromIso8601
                        |> expectJust
            , test "returns Nothing for invalid ISO8601 string" <|
                \_ ->
                    Date.fromIso8601 "foo"
                        |> Expect.equal Nothing
            , test "returns Nothing for invalid date" <|
                \_ ->
                    Date.fromIso8601 "2019-02-31"
                        |> Expect.equal Nothing
            ]
        , describe "Date.range"
            [ test "generates Dates including bounds" <|
                \_ ->
                    Date.range
                        (Date.fromYMD 2019 1 1)
                        (Date.fromYMD 2019 1 5)
                        |> Expect.equal
                            [ Date.fromYMD 2019 1 1
                            , Date.fromYMD 2019 1 2
                            , Date.fromYMD 2019 1 3
                            , Date.fromYMD 2019 1 4
                            , Date.fromYMD 2019 1 5
                            ]
            , test "generates Dates in ascending order even when boundary dates are swapped" <|
                \_ ->
                    Date.range
                        (Date.fromYMD 2019 1 5)
                        (Date.fromYMD 2019 1 1)
                        |> Expect.equal
                            [ Date.fromYMD 2019 1 1
                            , Date.fromYMD 2019 1 2
                            , Date.fromYMD 2019 1 3
                            , Date.fromYMD 2019 1 4
                            , Date.fromYMD 2019 1 5
                            ]
            , fuzz fuzzDate "returns single date for two same boundaries" <|
                \date ->
                    Date.range date date
                        |> Expect.equal [ date ]
            , test "supports different month boundaries" <|
                \_ ->
                    Date.range
                        (Date.fromYMD 2019 1 31)
                        (Date.fromYMD 2019 2 3)
                        |> Expect.equal
                            [ Date.fromYMD 2019 1 31
                            , Date.fromYMD 2019 2 1
                            , Date.fromYMD 2019 2 2
                            , Date.fromYMD 2019 2 3
                            ]
            , test "supports different year boundaries" <|
                \_ ->
                    Date.range
                        (Date.fromYMD 2019 12 31)
                        (Date.fromYMD 2020 1 3)
                        |> Expect.equal
                            [ Date.fromYMD 2019 12 31
                            , Date.fromYMD 2020 1 1
                            , Date.fromYMD 2020 1 2
                            , Date.fromYMD 2020 1 3
                            ]
            ]
        ]
