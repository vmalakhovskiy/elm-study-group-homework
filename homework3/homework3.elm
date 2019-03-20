--# Homework Week 3


module Main exposing (maybeToList)

import Date exposing (Date)
import List exposing (..)
import Maybe exposing (..)
import Result exposing (toMaybe)



--**maybeToList**


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Just data ->
            singleton data

        Nothing ->
            []



--**updateList**


updateList :
    (a -> Bool)
    -> (a -> Maybe a)
    -> List a
    -> List a
updateList shouldChange f xs =
    List.filterMap
        (\x ->
            if shouldChange x then
                f x

            else
                Just x
        )
        xs



--**find**


find : (a -> Bool) -> List a -> Maybe a
find f xs =
    xs
        |> filter f
        |> head



--**Implement updateListKv**
-- andMap :
--     Maybe a
--     -> Maybe (a -> value)
--     -> Maybe value
-- andMap =
--     Maybe.map2 (|>)


updateListKv :
    List ( k, v )
    -> k
    -> (v -> Maybe v)
    -> List ( k, v )
updateListKv old k f =
    List.filterMap
        (\( key, value ) ->
            if key == k then
                Maybe.map (Tuple.pair key) (f value)

            else
                Just ( key, value )
        )
        old



--**keepOks**


keepOks : List (Result a b) -> List b
keepOks =
    filterMap toMaybe



--**mapOk**


mapOk :
    (b -> c)
    -> Result a b
    -> Result a c
mapOk =
    Result.map



--**either**


either :
    (a -> c)
    -> (b -> c)
    -> Result a b
    -> c
either me mv r =
    case r of
        Ok v ->
            mv v

        Err e ->
            me e



--**Implement `parseDate`**


parseDate : Maybe String -> Maybe Date
parseDate =
    Maybe.andThen <| Date.fromIsoString >> Result.toMaybe



--**Timer**
--Implement "Timer" from http://eugenkiss.github.io/7guis/tasks/
--Some additional notes:
--- see html at https://guide.elm-lang.org/interop/ used together with
--  `elm make --output=out/YourApp.js` to compile to JS and be able to
--  include CSS like bootstrap in your timer app
--- use the `Time` module from `elm/time` pacakge to get current time
--  and subscribe to periodic updates
--  https://package.elm-lang.org/packages/elm/time/1.0.0/Time
--- use `Task.perform` function to convert a `Task` into a command.
--  For example:
--  ```elm
--  import Time
--  type Msg
--    ...
--    | GotInitialTime Time.Posix
--  update : Msg -> Model -> ( Model, Cmd Msg )
--  update =
--    ...
--    ( model
--    , Task.perform GotInitialTime Time.now
--    )
--    ...
--  ```
--  See https://package.elm-lang.org/packages/elm/core/latest/Task
--- use subscription mechanism to subscribe to periodic time changes. For example:
--  ```elm
--  ...
--  type Msg =
--    ...
--    | TimeUpdate Time.Posix
--  ...
--  subscriptions : Model -> Sub Msg
--  subscriptions model =
--      Time.every 200.0 TimeUpdate
--  main : Program () Model Msg
--  main =
--      Browser.element
--          { init = init
--          , view = view
--          , update = update
--          , subscriptions = subscriptions
--          }
--  ```
--**(optional) Graceful Labeling**
--Graceful Labeling from
--https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/p/p92.html
