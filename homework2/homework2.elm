module Main exposing
    ( bird
    , bird01
    , convert
    , convert02
    , convert03
    , result
    , transform
    , transform02
    , transform03
    , wrap
    )

import List exposing (..)
import Maybe exposing (withDefault)
import String exposing (..)
import Url.Builder
    exposing
        ( absolute
        , int
        , string
        )



--# Homework Week 2
--**Map one structure to another**


transform :
    { name : String, email : String, phone_number : String }
    -> { name : String, email : String }
transform data =
    { name = data.name, email = data.email }


convert :
    List { name : String, email : String, phone_number : String }
    -> List { name : String, email : String }
convert list =
    list |> List.map transform


result =
    convert [ { name = "string", email = "string", phone_number = "string" } ]



--**Filter elements with non-empty name and email**


transform02 :
    { name : Maybe String, email : Maybe String }
    -> Maybe { name : String, email : String }
transform02 data =
    case ( data.name, data.email ) of
        ( Just n, Just e ) ->
            Just { name = n, email = e }

        ( _, _ ) ->
            Nothing


convert02 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert02 =
    filterMap transform02



--**Fill in missing emails with `<unspecified>`, while removing elements


wrap : Maybe String -> String
wrap =
    Maybe.withDefault "<unspecified>"


transform03 :
    { name : Maybe String, email : Maybe String }
    -> { name : String, email : String }
transform03 data =
    { name = wrap data.name, email = wrap data.email }


convert03 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert03 data =
    data |> List.map transform03



--**Rewrite bird using `<|`, then using `|>` instead of parens (where applicable)**


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
        |> List.map incr
        |> List.filter notThree
        |> List.sum


bird01 : Int
bird01 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr <| [ 1, 2, 3 ]



--**Implement setPhone**


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone number user =
    { profile = { address = { phone = number } } }



--**mapMaybes**


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes =
    filterMap



--**catMaybes**


catMaybes : List (Maybe a) -> List a
catMaybes =
    filterMap identity



--**Use package [elm/url](https://package.elm-lang.org/packages/elm/url/latest) and its Url.Builder.absolute to build URL from parameters**


mapToRouteParameters :
    Int
    -> List String
mapToRouteParameters path =
    [ "api"
    , "item"
    , fromInt path
    , "stats.json"
    ]


mapToQueryParameters :
    { startDate : Maybe String, numElems : Maybe Int }
    -> List Url.Builder.QueryParameter
mapToQueryParameters query =
    catMaybes
        [ Maybe.map (string "start_date") query.startDate
        , Maybe.map (int "numElems") query.numElems
        ]


buildStatsUrl :
    Int
    -> { startDate : Maybe String, numElems : Maybe Int }
    -> String
buildStatsUrl path query =
    "https://myapi.com"
        ++ absolute
            (path |> mapToRouteParameters)
            (query |> mapToQueryParameters)
