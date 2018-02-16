module View exposing (view)

import Html exposing (div, ul, li, Html, text, button, img, button, Attribute, a)
import Html.Attributes exposing (src, disabled, class, href, maxlength)
import Html.Events exposing (onClick)
import Time exposing (Time, second, millisecond)
import Css
import Markdown
import SpotifyApi
import FM4Api
import Model exposing (Model, Msg(..))
import Routing exposing (Route(..))


styles =
    Css.asPairs >> Html.Attributes.style


view : Model -> Html Msg
view m =
    div
        [ styles
            [ Css.maxWidth (Css.px 500)
            , Css.margin Css.auto
            ]
        ]
        [ selectRouteView m ]


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs =
    button
        ([ class "btn btn-secondary"
         , styles
            [ Css.marginRight (Css.px 5)
            , Css.marginTop (Css.px 5)
            , Css.width (Css.px 76)
            , Css.height (Css.px 40)
            ]
         ]
            ++ attrs
        )


nav : Bool -> Html Msg
nav isSpotify =
    let
        ( fm4Active, spotifyActive ) =
            if isSpotify then
                ( "", " active" )
            else
                ( " active", "" )

        item active goTo msg =
            li [ class "nav-item" ]
                [ Html.span
                    [ class ("nav-link" ++ active)
                    , onClick goTo
                    , styles [ Css.fontSize (Css.px 20) ]
                    ]
                    [ text msg ]
                ]
    in
        ul [ class "nav nav-tabs" ]
            [ item spotifyActive GoToSpotify "Spotify"
            , item fm4Active GoToFm4 "FM4"
            ]


currentlyPlaying : SpotifyApi.Song -> Html Msg
currentlyPlaying song =
    div []
        [ img [ src song.imageUrl ] []
        , div [] [ text (song.name) ]
        , div [] [ text (song.artist) ]
        ]


nothingPlaying =
    div [] [ text "Nothing is played currently" ]


playlistsPart =
    div [] [ text "Playlists:" ]


highlightIfSongIsPlayingIsIn : SpotifyApi.Playlist -> Maybe SpotifyApi.Song -> List (Attribute Msg)
highlightIfSongIsPlayingIsIn playlist song =
    case song of
        Just s ->
            if List.any (\id -> id == s.id) playlist.songs then
                [ styles [ Css.borderColor (Css.rgb 216 2 32) ], onClick (RemoveFromPlaylist playlist s) ]
            else
                [ onClick (AddToPlaylist playlist s) ]

        Nothing ->
            [ disabled True ]


playlistView : List SpotifyApi.Playlist -> Maybe SpotifyApi.Song -> Bool -> List (Html Msg)
playlistView playlists songPlaying withButton =
    let
        playlistButton playlistId =
            btn [ onClick (PlayPlaylist playlistId) ] [ Markdown.toHtml [] "&#9654;" ]
    in
        playlists
            |> List.map
                (\playlist ->
                    div []
                        ([ btn (highlightIfSongIsPlayingIsIn playlist songPlaying) [ text playlist.name ]
                         ]
                            ++ if withButton then
                                [ playlistButton playlist.id ]
                               else
                                []
                        )
                )


playingBox : List (Html msg) -> Html msg
playingBox elements =
    div
        [ styles
            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
            , Css.borderLeft3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
            , Css.borderRight3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
            , Css.borderRadius (Css.px 2)
            ]
        ]
        [ div [ styles [ Css.marginBottom (Css.px 10) ] ] elements ]


selectRouteView : Model -> Html Msg
selectRouteView m =
    case m.routes of
        Home ->
            div [] [ text "Being redirected" ]

        NotFoundRoute ->
            notFoundView

        SpotifyView ->
            let
                controls =
                    div []
                        [ btn [ onClick Previous ] [ text "<<" ]
                        , btn [ onClick Play ] [ Markdown.toHtml [] "&#9654;" ]
                        , btn [ onClick Pause ] [ Markdown.toHtml [] "&#10074;&#10074;" ]
                        , btn [ onClick Next ] [ text ">>" ]
                        ]
            in
                div []
                    [ nav True
                    , playingBox
                        ((m.songPlaying
                            |> Maybe.map currentlyPlaying
                            |> Maybe.map (\playing -> div [] [ playing, controls ])
                            |> Maybe.withDefault nothingPlaying
                         )
                            :: playlistsPart
                            :: playlistView m.playlists m.songPlaying True
                        )
                    ]

        Fm4View ->
            div []
                [ nav False
                , playingBox
                    ((m.fm4SongPlayingInSpotify
                        |> Maybe.map currentlyPlaying
                        |> Maybe.withDefault nothingPlaying
                     )
                        :: playlistsPart
                        :: playlistView m.playlists m.fm4SongPlayingInSpotify False
                    )
                ]

        Authenticated _ ->
            div [] [ text "Authenticat" ]

        AuthenticationFailed ->
            div [] [ text "Authentication failed" ]


notFoundView : Html msg
notFoundView =
    div [] [ text "Not Found" ]
