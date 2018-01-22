module Main exposing (..)
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Debug
import Time

type alias Model = {currentMonth : Maybe Date.Date}
type Msg = Today Date.Date | Next | Prev

initial : ( Model, Cmd Msg )
initial = {currentMonth=Nothing} ! [Task.perform Today Date.now]

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case msg of
        Today td ->
            ({m|currentMonth=(toFirstDay td |> Debug.log "today" |> Just)} , Cmd.none)

        Next ->
            case m.currentMonth of
                Just day ->
                    ( {m|currentMonth=(nextMonth day |> Debug.log "next month" |> Just) }
                    , Cmd.none)
                Nothing -> (m, Cmd.none)

        Prev ->
            case m.currentMonth of
                Just day ->
                    ( {m|currentMonth=(prevMonth day |> Debug.log "next month" |> Just) }
                    , Cmd.none)
                Nothing -> (m, Cmd.none)


view m =
    div [] [ text "aa"
           , button [onClick Prev] [text "前"]
           , button [onClick Next] [text "次"]
           ]

subscriptions : Model -> Sub Msg
subscriptions m = Sub.none


main : Program Never Model Msg
main =
    program
        { init = initial
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

minusDay : Date.Date -> Int -> Date.Date
minusDay date day =
    (Date.toTime date) - (Time.hour * 24 * (toFloat day)) |> Date.fromTime


plusDay : Date.Date -> Int -> Date.Date
plusDay date day =
    (Date.toTime date) + (Time.hour * 24 * (toFloat day)) |> Date.fromTime


toFirstDay : Date.Date -> Date.Date
toFirstDay d =
    minusDay d (Date.day d - 1)

isUru : Int -> Bool
isUru year =
    if rem year 400 == 0 then
        True
    else if rem year 100 == 0 then
        False
    else if rem year 4 == 0 then
        True
    else
        False

getDaysOfMonth : Date.Date -> Int
getDaysOfMonth d =
    let
        month = Date.month d
    in
        case month of
            Date.Jan -> 31
            Date.Mar -> 31
            Date.Apr -> 30
            Date.May -> 31
            Date.Jun -> 30
            Date.Jul -> 31
            Date.Aug -> 31
            Date.Sep -> 30
            Date.Oct -> 31
            Date.Nov -> 30
            Date.Dec -> 31
            Date.Feb -> if isUru (Date.year d) then 29 else 28

nextMonth : Date.Date -> Date.Date
nextMonth d =
    getDaysOfMonth d |> plusDay d

prevMonth : Date.Date -> Date.Date
prevMonth d =
    minusDay d 1 |> toFirstDay
