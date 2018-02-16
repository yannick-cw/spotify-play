module Routing exposing (Route(..), parseLocation)

import Navigation exposing (Location)
import UrlParser exposing (map, top, Parser, parsePath, oneOf, s, (<?>), string, stringParam)


type Route
    = Home
    | Authenticated String
    | SpotifyView
    | Fm4View
    | AuthenticationFailed
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map maybeAuth (s "authenticated" <?> stringParam "access_token")
        , map SpotifyView (s "spotify")
        , map Fm4View (s "fm4")
        ]


maybeAuth : Maybe String -> Route
maybeAuth maybeToken =
    case maybeToken of
        Just tk ->
            Authenticated tk

        Nothing ->
            AuthenticationFailed


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers { location | search = location.hash }) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
