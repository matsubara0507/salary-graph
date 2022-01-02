module Generated.SalaryAPI exposing (..)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


type alias Salary =
    { year : Year
    , month : Month
    , gross : Int
    , net : Int
    , isBonus : Bool
    }


jsonDecSalary : Json.Decode.Decoder Salary
jsonDecSalary =
    Json.Decode.succeed (\pyear pmonth pgross pnet pisBonus -> { year = pyear, month = pmonth, gross = pgross, net = pnet, isBonus = pisBonus })
        |> required "year" jsonDecYear
        |> required "month" jsonDecMonth
        |> required "gross" Json.Decode.int
        |> required "net" Json.Decode.int
        |> required "isBonus" Json.Decode.bool


jsonEncSalary : Salary -> Value
jsonEncSalary val =
    Json.Encode.object
        [ ( "year", jsonEncYear val.year )
        , ( "month", jsonEncMonth val.month )
        , ( "gross", Json.Encode.int val.gross )
        , ( "net", Json.Encode.int val.net )
        , ( "isBonus", Json.Encode.bool val.isBonus )
        ]


type alias Year =
    Int


jsonDecYear : Json.Decode.Decoder Year
jsonDecYear =
    Json.Decode.int


jsonEncYear : Year -> Value
jsonEncYear val =
    Json.Encode.int val


type alias Month =
    Int


jsonDecMonth : Json.Decode.Decoder Month
jsonDecMonth =
    Json.Decode.int


jsonEncMonth : Month -> Value
jsonEncMonth val =
    Json.Encode.int val


type alias Appointment =
    { year : Year
    , month : Month
    , before : Int
    , after : Int
    }


jsonDecAppointment : Json.Decode.Decoder Appointment
jsonDecAppointment =
    Json.Decode.succeed (\pyear pmonth pbefore pafter -> { year = pyear, month = pmonth, before = pbefore, after = pafter })
        |> required "year" jsonDecYear
        |> required "month" jsonDecMonth
        |> required "before" Json.Decode.int
        |> required "after" Json.Decode.int


jsonEncAppointment : Appointment -> Value
jsonEncAppointment val =
    Json.Encode.object
        [ ( "year", jsonEncYear val.year )
        , ( "month", jsonEncMonth val.month )
        , ( "before", Json.Encode.int val.before )
        , ( "after", Json.Encode.int val.after )
        ]


getApiSalariesByYear : Year -> (Result Http.Error (List Salary) -> msg) -> Cmd msg
getApiSalariesByYear capture_year toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "salaries"
                , capture_year |> String.fromInt
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecSalary)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiAppointmentsByYear : Year -> (Result Http.Error (List Appointment) -> msg) -> Cmd msg
getApiAppointmentsByYear capture_year toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "appointments"
                , capture_year |> String.fromInt
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecAppointment)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
