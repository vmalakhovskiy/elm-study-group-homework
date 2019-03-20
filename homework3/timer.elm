module Main exposing (main)

import Browser
import Html exposing (Html, br, input, label, p)
import Html.Attributes exposing (checked)
import Html.Events exposing (onCheck)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Posix
    , zone : Time.Zone
    , sub : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Time.millisToPosix 0) Time.utc True, Task.perform Zone Time.here )



-- UPDATE


type Msg
    = Tick Posix
    | ClockSub Bool
    | Zone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        ClockSub state ->
            ( { model | sub = state }, Cmd.none )

        Zone zone ->
            ( { model | zone = zone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.sub then
        Time.every 1000 Tick

    else
        Sub.none



-- VIEW


formatTime zone posix =
    (String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone posix)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone posix)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone posix)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.p [] [ text "Turn subscription on or off" ]
        , text <| formatTime model.zone model.time
        , radio True "on" model
        , radio False "off" model
        , br [] []
        , clock model
        ]


radio : Bool -> String -> Model -> Html Msg
radio state name model =
    let
        isSelected =
            model.sub == state
    in
    label []
        [ br [] []
        , input [ type_ "radio", checked isSelected, onCheck (\_ -> ClockSub state) ] []
        , text name
        ]


startingAngle =
    90


secondToDegree : Time.Zone -> Time.Posix -> Float
secondToDegree zone posix =
    let
        seconds =
            toFloat <| Time.toSecond zone posix
    in
    360 - (360 / 60) * seconds + startingAngle


minuteToDegree : Time.Zone -> Time.Posix -> Float
minuteToDegree zone posix =
    let
        minutesSeconds =
            (toFloat <| Time.toMinute zone posix) + (toFloat <| Time.toSecond zone posix) / 60
    in
    360 - (360 / 60) * minutesSeconds + startingAngle


hourToDegree : Time.Zone -> Time.Posix -> Float
hourToDegree zone posix =
    let
        hour12 =
            toFloat <| remainderBy 12 (Time.toHour zone posix)

        hoursMinutesSeconds =
            hour12 + (toFloat <| Time.toMinute zone posix) / 60 + (toFloat <| Time.toSecond zone posix) / 60 / 60
    in
    360 - (360 / 12) * hoursMinutesSeconds + startingAngle


hand offset length angle =
    ( offset + length * cos angle
    , offset + length * -1 * sin angle
    )


handLine offset ( x2_, y2_ ) =
    line
        [ x1 <| String.fromInt offset
        , y1 <| String.fromInt offset
        , x2 <| String.fromFloat x2_
        , y2 <| String.fromFloat y2_
        , stroke "#023963"
        ]
        []


clock : Model -> Html msg
clock model =
    let
        handSecond =
            hand 50 40 <| degrees <| secondToDegree model.zone model.time

        handMinute =
            hand 50 30 <| degrees <| minuteToDegree model.zone model.time

        handHour =
            hand 50 20 <| degrees <| hourToDegree model.zone model.time
    in
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , handLine 50 handSecond
        , handLine 50 handMinute
        , handLine 50 handHour
        ]
