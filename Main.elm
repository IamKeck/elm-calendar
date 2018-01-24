module Main exposing (..)
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Debug
import Time
import List
import MyDate

type alias Model = {currentMonth : Maybe Date.Date, clickedDate : Maybe Date.Date}
type Msg = Today Date.Date | Next | Prev | DateClicked Date.Date

initial : ( Model, Cmd Msg )
initial = {currentMonth=Nothing, clickedDate=Nothing} ! [Task.perform Today Date.now]

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case msg of
        Today td ->
            (MyDate.toFirstDay td |> updateMonth m, Cmd.none)

        Next ->
            case m.currentMonth of
                Just day ->
                    ( updateMonth m <| MyDate.nextMonth day
                    , Cmd.none)
                Nothing -> (m, Cmd.none)

        Prev ->
            case m.currentMonth of
                Just day ->
                    ( updateMonth m <| MyDate.prevMonth day
                    , Cmd.none)
                Nothing -> (m, Cmd.none)

        DateClicked d ->
            {m| clickedDate=Just d} ! []


updateMonth : Model -> Date.Date -> Model
updateMonth m first_day =
    Debug.log "updateMonth!" {m| currentMonth = Just first_day}


createDayList : Date.Date -> Date.Date -> List Date.Date -> List Date.Date
createDayList start_day current_day acc =
    if current_day == start_day then
        start_day :: acc
    else
        current_day :: acc |> createDayList start_day (MyDate.minusDay current_day 1)


createCalendar : Date.Month -> Maybe Date.Date -> List Date.Date -> Html Msg
createCalendar mon clicked_date date_list =
    let
        head =
            List.map (\w -> text w |> List.singleton |> td [] ) ["日", "月", "火", "水", "木", "金", "土"]
            |> tr [] |> List.singleton |> thead []
        body = List.foldr createCalendarInner [] date_list |> List.map (createCalendarRow mon clicked_date) |> tbody []
    in
        table [] [head, body]


createCalendarRow : Date.Month -> Maybe Date.Date -> List Date.Date -> Html Msg
createCalendarRow mon clicked_date ds =
    let
        create_cell =
            \d ->
                let
                    is_out_month =
                        mon /= Date.month d
                in
                    td [ classList [ ("out", is_out_month)
                                   , ("clicked_date", clicked_date == Just d)]

                        , onClick (DateClicked d)]
                       [Date.day d |> toString |> text]
    in
        List.map create_cell ds |> tr []


createCalendarInner : Date.Date ->  List (List Date.Date) ->   List (List Date.Date)
createCalendarInner d acc =
    case acc of
        [] ->
            [[d]]
        ac::acs ->
            case Date.dayOfWeek d of
                Date.Sat ->
                    [d]::(ac::acs)
                _ ->
                    (d::ac)::acs


view m =
    let
        current_month =
            case m.currentMonth of
                Nothing -> Date.Jan
                Just day -> Date.month day
        calendar_elm =
            case m.currentMonth of
                Nothing -> []
                Just day ->
                    let
                        start_day = MyDate.getStartDay day
                        last_day = MyDate.toLastDay day |>  MyDate.getEndDay
                    in createDayList start_day last_day [] |> createCalendar current_month m.clickedDate
                       |> List.singleton

    in
        div [] <| [ toString current_month |> text
                  , button [onClick Prev] [text "前"]
                  , button [onClick Next] [text "次"]
                  ] ++ calendar_elm

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

