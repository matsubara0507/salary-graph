module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Dict exposing (Dict)
import Generated.SalaryAPI as Salary exposing (Salary)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List
import Model.Monthly as Monthly
import Model.Range as Range exposing (Range)
import Task
import Time exposing (Posix)
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , range : Range
    , salaries : Dict Salary.Year (List Salary)
    , appointments : Dict Salary.Year (List Salary.Appointment)
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FetchSalaries Salary.Year (Result Http.Error (List Salary))
    | FetchAppointments Salary.Year (Result Http.Error (List Salary.Appointment))
    | HereZone Time.Zone
    | Now Posix


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , range = Range.empty
        , salaries = Dict.empty
        , appointments = Dict.empty
        }


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl _ model =
    ( model, Cmd.batch [ Task.perform HereZone Time.here, Task.perform Now Time.now ] )


fetchAll : Model -> ( Model, Cmd Msg )
fetchAll model =
    ( model, Cmd.batch [ fetchSalaries model.range, fetchAppointments model.range ] )


fetchSalaries : Range -> Cmd Msg
fetchSalaries range =
    Range.yearRange range
        |> List.map (\y -> Salary.getApiSalariesByYear y (FetchSalaries y))
        |> Cmd.batch


fetchAppointments : Range -> Cmd Msg
fetchAppointments range =
    Range.yearRange range
        |> List.map (\y -> Salary.getApiAppointmentsByYear y (FetchAppointments y))
        |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        FetchSalaries year (Ok salaries) ->
            ( { model | salaries = Dict.insert year salaries model.salaries }, Cmd.none )

        FetchSalaries _ (Err _) ->
            ( model, Cmd.none )

        FetchAppointments year (Ok appointments) ->
            ( { model | appointments = Dict.insert year appointments model.appointments }, Cmd.none )

        FetchAppointments _ (Err _) ->
            ( model, Cmd.none )

        HereZone zone ->
            ( { model | range = Range.updateZone zone model.range }, Cmd.none )

        Now t ->
            fetchAll { model | range = Range.syncYearMonth t model.range }


view : Model -> Browser.Document Msg
view model =
    { title = "Salary Graph"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ div [ class "Header" ]
        [ div [ class "Header-item" ] [ span [] [ text "Salary Graph" ] ]
        ]
    , div [ class "col-8 mx-auto text-center m-6" ]
        [ viewChart model
        ]
    ]


viewChart : Model -> Html Msg
viewChart model =
    let
        monthlies =
            Monthly.build
                (List.concat (Dict.values model.salaries))
                (List.concat (Dict.values model.appointments))
                |> List.sortBy .x

        maxHeight =
            List.map .gross monthlies
                |> List.maximum
                |> Maybe.map (\x -> toFloat (ceiling (x / 10000) * 10000))
                |> Maybe.withDefault 0
    in
    C.chart
        [ CA.height 600
        , CA.width 1200
        , CA.domain
            [ CA.highest maxHeight CA.exactly
            ]
        , CA.margin { top = 30, bottom = 0, left = 0, right = 0 }
        ]
        [ C.yLabels [ CA.withGrid, CA.pinned .min ]
        , C.binLabels
            (\x -> String.fromInt x.year ++ "/" ++ String.fromInt x.month)
            [ CA.moveDown 15 ]
        , C.bars
            [ CA.roundTop 0.3, CA.ungroup ]
            [ C.named "額面収入" <| C.bar .gross []
            , C.named "手取り収入" <| C.bar .net [ CA.striped [] ]
            ]
            monthlies
        , C.series .x
            [ C.named "基本給" <|
                C.interpolated .base
                    [ CA.width 4 ]
                    [ CA.cross, CA.borderWidth 2, CA.border "white" ]
            ]
            monthlies
        , C.legendsAt .min
            .max
            [ CA.row
            , CA.moveRight 10
            , CA.moveUp 50
            , CA.spacing 15
            ]
            [ CA.width 20 ]
        ]
