module Model.Monthly exposing (Monthly, build)

import Dict
import Generated.SalaryAPI as Salary exposing (Salary)
import List
import List.Ext as List


type alias Monthly =
    { x : Float
    , year : Int
    , month : Int
    , totalGross : Float
    , totalNet : Float
    , gross : Float
    , net : Float
    , base : Float
    }


build : List Salary -> List Salary.Appointment -> List Monthly
build salaries appointments =
    let
        bonuses =
            List.filter .isBonus salaries

        sortedAppointments =
            List.sortBy (\x -> x.year * 100 + x.month) appointments
    in
    List.filter (\x -> not x.isBonus) salaries
        |> List.sortBy (\x -> x.year * 100 + x.month)
        |> List.indexedMap (fromSalary sortedAppointments)
        |> List.map (applyBonuses bonuses)



-- | expected appointments is sorted


fromSalary : List Salary.Appointment -> Int -> Salary -> Monthly
fromSalary appointments idx salary =
    let
        startBase =
            List.head appointments
                |> Maybe.map .before
                |> Maybe.withDefault 0

        ym =
            salary.year * 100 + salary.month

        latestBase =
            appointments
                |> List.reverse
                |> List.find (\x -> x.year * 100 + x.month < ym)
                |> Maybe.map .after
                |> Maybe.withDefault startBase
    in
    { x = toFloat (idx + 1)
    , year = salary.year
    , month = salary.month
    , totalGross = toFloat salary.gross
    , totalNet = toFloat salary.net
    , gross = toFloat salary.gross
    , net = toFloat salary.net
    , base = toFloat latestBase
    }


applyBonuses : List Salary -> Monthly -> Monthly
applyBonuses bonuses monthly =
    let
        target =
            List.filter (\x -> x.year == monthly.year && x.month == monthly.month) bonuses

        bonusGross =
            List.map .gross target
                |> List.sum
                |> toFloat

        bonusNet =
            List.map .net target
                |> List.sum
                |> toFloat
    in
    { monthly
        | totalGross = monthly.totalGross + bonusGross
        , totalNet = monthly.totalNet + bonusNet
    }
