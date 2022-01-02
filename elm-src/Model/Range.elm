module Model.Range exposing (..)

import Generated.SalaryAPI as Salary
import List
import Time exposing (Month(..), Posix)
import Time.Extra as Time
import Tuple


type alias Range =
    { from : ( Salary.Year, Salary.Month )
    , to : ( Salary.Year, Salary.Month )
    , zone : Time.Zone
    }


empty : Range
empty =
    { from = ( 0, 0 ), to = ( 0, 0 ), zone = Time.utc }


updateZone : Time.Zone -> Range -> Range
updateZone zone range =
    { range | zone = zone }


yearRange : Range -> List Salary.Year
yearRange range =
    List.range (Tuple.first range.from) (Tuple.first range.to)


syncYearMonth : Posix -> Range -> Range
syncYearMonth toTime range =
    let
        fromTime =
            toTime
                |> Time.add Time.Year -1 range.zone
                |> Time.add Time.Day 1 range.zone
    in
    { range
        | from = toYearMonth fromTime range.zone
        , to = toYearMonth toTime range.zone
    }


toYearMonth : Posix -> Time.Zone -> ( Salary.Year, Salary.Month )
toYearMonth t zone =
    let
        month =
            case Time.toMonth zone t of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12
    in
    ( Time.toYear zone t, month )
