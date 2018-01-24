module Main exposing (..)
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Debug
import Time
import List

type alias Model = {currentMonth : Maybe Date.Date}
type Msg = Today Date.Date | Next | Prev

initial : ( Model, Cmd Msg )
initial = {currentMonth=Nothing} ! [Task.perform Today Date.now]

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case msg of
        Today td ->
            (toFirstDay td |> updateMonth m, Cmd.none)

        Next ->
            case m.currentMonth of
                Just day ->
                    ( updateMonth m <| nextMonth day
                    , Cmd.none)
                Nothing -> (m, Cmd.none)

        Prev ->
            case m.currentMonth of
                Just day ->
                    ( updateMonth m <| prevMonth day
                    , Cmd.none)
                Nothing -> (m, Cmd.none)

updateMonth : Model -> Date.Date -> Model
updateMonth m first_day =
    Debug.log "updateMonth!" {m| currentMonth = Just first_day}


createDayList : Date.Date -> List Date.Date
createDayList first_day =
    List.foldr (\d acc -> plusDay first_day d :: acc) [] (List.range 0 34)

createCalendar : List Date.Date -> Html Msg
createCalendar date_list =
    List.foldr createCalendarInner [] date_list |> List.map createCalendarRow |> table []



createCalendarRow : List Date.Date -> Html Msg
createCalendarRow ds =
    List.map (\d -> td [] [text <| dayToString d]) ds |> tr []

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
        calendar_elm =
            case m.currentMonth of
                Nothing -> []
                Just day -> [getStartDay day |> createDayList |> createCalendar]

    in
        div [] <| [ text "aa"
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


weekToNum : Date.Day -> Int
weekToNum w =
    case w of
        Date.Sun -> 0
        Date.Mon -> 1
        Date.Tue -> 2
        Date.Wed -> 3
        Date.Thu -> 4
        Date.Fri -> 5
        Date.Sat -> 6

getStartDay : Date.Date -> Date.Date
getStartDay first_day =
    let
        week_num = Date.dayOfWeek first_day |> weekToNum
    in
        minusDay first_day week_num


dayToString : Date.Date -> String
dayToString d =
    let
        month_int =
            case Date.month d of
                Date.Jan -> 1
                Date.Feb -> 2
                Date.Mar -> 3
                Date.Apr -> 4
                Date.May -> 5
                Date.Jun -> 6
                Date.Jul -> 7
                Date.Aug -> 8
                Date.Sep -> 9
                Date.Oct -> 10
                Date.Nov -> 11
                Date.Dec -> 12
    in
        (toString <| Date.year d) ++ "年" ++ (toString <| month_int) ++ "月" ++ (toString <| Date.day d) ++ "日"


