module MyDate exposing(..)
import Date
import Time


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


