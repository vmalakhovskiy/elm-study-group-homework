module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , toCelsius
    , toFahrenheit
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { celsius : Maybe String
    , fahrenheit : Maybe String
    }


init : Model
init =
    { celsius = Nothing
    , fahrenheit = Nothing
    }



-- UPDATE


type Msg
    = Celsius String
    | Fahrenheit String


toCelsius : Float -> Float
toCelsius f =
    (f - 32) * 5 / 9


convertToCelsius : String -> Maybe String
convertToCelsius fahrenheit =
    fahrenheit
        |> String.toFloat
        |> Maybe.map toCelsius
        |> Maybe.map String.fromFloat


toFahrenheit : Float -> Float
toFahrenheit c =
    c * 9 / 5 + 32


convertToFahrenheit : String -> Maybe String
convertToFahrenheit celsius =
    celsius
        |> String.toFloat
        |> Maybe.map toFahrenheit
        |> Maybe.map String.fromFloat


update : Msg -> Model -> Model
update msg model =
    case msg of
        Celsius value ->
            { celsius = Just value
            , fahrenheit = value |> convertToFahrenheit
            }

        Fahrenheit value ->
            { celsius = value |> convertToCelsius
            , fahrenheit = Just value
            }



-- VIEW


wrap : Maybe String -> String
wrap =
    Maybe.withDefault ""


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , placeholder "Celsius"
            , value (model.celsius |> wrap)
            , onInput Celsius
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Fahrenheit"
            , value (model.fahrenheit |> wrap)
            , onInput Fahrenheit
            ]
            []
        ]
