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

type alias Model = {currentMonth : Maybe Date.Date}
type Msg = Today Date.Date | Next | Prev

initial : ( Model, Cmd Msg )
initial = {currentMonth=Nothing} ! [Task.perform Today Date.now]

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

updateMonth : Model -> Date.Date -> Model
updateMonth m first_day =
    Debug.log "updateMonth!" {m| currentMonth = Just first_day}


createDayList : Date.Date -> List Date.Date
createDayList first_day =
    List.foldr (\d acc -> MyDate.plusDay first_day d :: acc) [] (List.range 0 34)

createCalendar : List Date.Date -> Html Msg
createCalendar date_list =
    List.foldr createCalendarInner [] date_list |> List.map createCalendarRow |> table []



createCalendarRow : List Date.Date -> Html Msg
createCalendarRow ds =
    List.map (\d -> td [] [text <| MyDate.dayToString d]) ds |> tr []

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
                Just day -> [MyDate.getStartDay day |> createDayList |> createCalendar]

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

