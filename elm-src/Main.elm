module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Dict exposing (Dict)
import Generated.SalaryAPI as Salary exposing (Salary)
import Html exposing (..)
import Html.Attributes exposing (checked, class, maxlength, size, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
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
    , inputFrom : String
    , inputTo : String
    , displayLabel : Bool
    , displayNet : Bool
    , displayBase : Bool
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FetchSalaries Salary.Year (Result Http.Error (List Salary))
    | FetchAppointments Salary.Year (Result Http.Error (List Salary.Appointment))
    | HereZone Time.Zone
    | Now Posix
    | InputFrom String
    | InputTo String
    | ChangeRange
    | DisplayLabel Bool
    | DisplayNet Bool
    | DisplayBase Bool


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , range = Range.empty
        , salaries = Dict.empty
        , appointments = Dict.empty
        , inputFrom = ""
        , inputTo = ""
        , displayLabel = True
        , displayNet = True
        , displayBase = True
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
            let
                range =
                    Range.syncYearMonth t model.range

                ( inputFrom, inputTo ) =
                    Range.toString range
            in
            fetchAll { model | range = range, inputFrom = inputFrom, inputTo = inputTo }

        InputFrom str ->
            ( { model | inputFrom = str }, Cmd.none )

        InputTo str ->
            ( { model | inputTo = str }, Cmd.none )

        ChangeRange ->
            case Range.fromString ( model.inputFrom, model.inputTo ) model.range.zone of
                Just range ->
                    fetchAll { model | range = range }

                _ ->
                    ( model, Cmd.none )

        DisplayLabel enable ->
            ( { model | displayLabel = enable }, Cmd.none )

        DisplayNet enable ->
            ( { model | displayNet = enable }, Cmd.none )

        DisplayBase enable ->
            ( { model | displayBase = enable }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Salary Graph"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "Layout Layout--gutter-condensed height-full" ]
        [ div [ class "Layout-sidebar SideNav border", style "max-width" "360px" ]
            []
        , div [ class "Layout-main" ]
            [ div [ class "container-lg p-3" ]
                [ div [ class "Subhead" ]
                    [ div [ class "Subhead-heading" ] [ text "Salary Graph" ]
                    , div [ class "Subhead-actions" ]
                        [ label [ class "p-1" ]
                            [ input [ type_ "checkbox", checked model.displayLabel, onCheck DisplayLabel ] []
                            , text "Y????????????"
                            ]
                        , label [ class "p-1" ]
                            [ input [ type_ "checkbox", checked model.displayNet, onCheck DisplayNet ] []
                            , text "????????????????????????"
                            ]
                        , label [ class "p-1" ]
                            [ input [ type_ "checkbox", checked model.displayBase, onCheck DisplayBase ] []
                            , text "??????????????????"
                            ]
                        , label [] [ text "From" ]
                        , input
                            [ class "form-control input-sm"
                            , type_ "text"
                            , size 6
                            , maxlength 6
                            , value model.inputFrom
                            , onInput InputFrom
                            ]
                            []
                        , label [] [ text "To" ]
                        , input
                            [ class "form-control input-sm"
                            , type_ "text"
                            , size 6
                            , maxlength 6
                            , value model.inputTo
                            , onInput InputTo
                            ]
                            []
                        , button [ class "btn btn-sm", type_ "submit", onClick ChangeRange ] [ text "??????" ]
                        ]
                    ]
                , div [ class "???mx-auto text-center m-6" ]
                    [ viewChart model
                    ]
                ]
            ]
        ]


viewChart : Model -> Html Msg
viewChart model =
    let
        monthlies =
            Monthly.build
                (List.filter (Range.include model.range) <| List.concat (Dict.values model.salaries))
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
        [ if model.displayLabel then
            C.yLabels [ CA.withGrid, CA.pinned .min ]

          else
            C.list []
        , C.binLabels
            (\x -> String.fromInt x.year ++ "/" ++ String.fromInt x.month)
            [ CA.moveDown 30
            , CA.rotate -45
            ]
        , C.bars
            [ CA.roundTop 0.3, CA.ungroup ]
            [ C.named "????????????" <| C.bar .gross []
            , if model.displayNet then
                C.named "???????????????" <| C.bar .net [ CA.striped [] ]

              else
                C.stacked []
            ]
            monthlies
        , if model.displayBase then
            C.series .x
                [ C.named "?????????" <|
                    C.interpolated .base
                        [ CA.width 4 ]
                        [ CA.cross, CA.borderWidth 2, CA.border "white" ]
                ]
                monthlies

          else
            C.list []
        , C.legendsAt .min
            .max
            [ CA.row
            , CA.moveRight 10
            , CA.moveUp 50
            , CA.spacing 15
            ]
            [ CA.width 20 ]
        ]
