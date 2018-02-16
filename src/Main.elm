module Main exposing (..)

import Html exposing (div, ul, li, Html, text, button, img, button, Attribute, a)
import Html.Attributes exposing (src, disabled, class, href, maxlength)
import Html.Events exposing (onClick)
import Navigation exposing (Location, load, newUrl, modifyUrl)
import Routing exposing (Route(..), parseLocation)
import Maybe exposing (withDefault)
import Http
import SpotifyApi
import Time exposing (Time, second, millisecond)
import Css
import Markdown
import Pagination exposing (paginate)
import Delay exposing (after)
import Dict
import FM4Api exposing (lastPlayingSong)


type alias Model =
    { routes : Route
    , songPlaying : Maybe SpotifyApi.Song
    , fm4SongPlaying : Maybe FM4Api.Song
    , fm4SongPlayingInSpotify : Maybe SpotifyApi.Song
    , playlists : List SpotifyApi.Playlist
    , userId : String
    , token : String
    }


type Msg
    = OnLocationChange Location
    | CurrentlyPlaying (Result Http.Error SpotifyApi.Song)
    | CurrentlyFm4Playing (Result Http.Error (Maybe FM4Api.Song))
    | Playlists (Result Http.Error (List SpotifyApi.Playlist))
    | PlaylistTracks String (Result Http.Error (List String))
    | FoundTracks (Result Http.Error (Maybe SpotifyApi.Song))
    | TogglePlay (Result Http.Error String)
    | Me (Result Http.Error String)
    | PlaylistChange (Result Http.Error String)
    | Play
    | PlayPlaylist String
    | Pause
    | Next
    | Previous
    | AddToPlaylist SpotifyApi.Playlist SpotifyApi.Song
    | RemoveFromPlaylist SpotifyApi.Playlist SpotifyApi.Song
    | UpdateCurrentlyPlaying
    | Tick Time
    | GoToFm4
    | GoToSpotify


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init (Model Home Nothing Nothing Nothing [] "" "")
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * second) Tick


view : Model -> Html Msg
view m =
    div
        [ styles
            [ Css.maxWidth (Css.px 500)
            , Css.margin Css.auto
            ]
        ]
        [ selectRouteView m ]


styles =
    Css.asPairs >> Html.Attributes.style


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


selectRouteView : Model -> Html Msg
selectRouteView m =
    case m.routes of
        Home ->
            div [] [ text "Being redirected" ]

        NotFoundRoute ->
            notFoundView

        SpotifyView ->
            let
                header =
                    ul [ class "nav nav-tabs" ]
                        [ li [ class "nav-item" ] [ a [ class "nav-link active", onClick GoToSpotify ] [ text "Spotify" ] ]
                        , li [ class "nav-item" ] [ a [ class "nav-link", onClick GoToFm4 ] [ text "FM4" ] ]
                        ]

                currentlyPlaying =
                    Maybe.map
                        (\song ->
                            div []
                                [ img [ src song.imageUrl ] []
                                , div [] [ text (song.name) ]
                                , div [] [ text (song.artist) ]
                                , btn [ onClick Previous ] [ text "<<" ]
                                , btn [ onClick Play ] [ Markdown.toHtml [] "&#9654;" ]
                                , btn [ onClick Pause ] [ Markdown.toHtml [] "&#10074;&#10074;" ]
                                , btn [ onClick Next ] [ text ">>" ]
                                ]
                        )

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

                playlistButton playlistId =
                    btn [ onClick (PlayPlaylist playlistId) ] [ Markdown.toHtml [] "&#9654;" ]

                playlistView =
                    m.playlists
                        |> List.map
                            (\playlist ->
                                div []
                                    [ btn (highlightIfSongIsPlayingIsIn playlist m.songPlaying) [ text playlist.name ]
                                    , playlistButton playlist.id
                                    ]
                            )
            in
                div []
                    [ header
                    , div
                        [ styles
                            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderLeft3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderRight3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderRadius (Css.px 2)
                            ]
                        ]
                        ((Maybe.withDefault nothingPlaying (currentlyPlaying m.songPlaying))
                            :: playlistsPart
                            :: playlistView
                        )
                    ]

        Fm4View ->
            let
                header =
                    ul [ class "nav nav-tabs" ]
                        [ li [ class "nav-item" ] [ a [ class "nav-link", onClick GoToSpotify ] [ text "Spotify" ] ]
                        , li [ class "nav-item" ] [ a [ class "nav-link active", onClick GoToFm4 ] [ text "FM4" ] ]
                        ]

                currentlyPlaying =
                    Maybe.map
                        (\song ->
                            div []
                                [ img [ src song.imageUrl ] []
                                , div [] [ text (song.name) ]
                                , div [] [ text (song.artist) ]
                                ]
                        )

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

                playlistView =
                    m.playlists
                        |> List.map
                            (\playlist ->
                                div []
                                    [ btn (highlightIfSongIsPlayingIsIn playlist m.fm4SongPlayingInSpotify) [ text playlist.name ]
                                    ]
                            )
            in
                div []
                    [ header
                    , div
                        [ styles
                            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderLeft3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderRight3 (Css.px 1) Css.solid (Css.rgb 221 221 221)
                            , Css.borderRadius (Css.px 2)
                            ]
                        ]
                        ((Maybe.withDefault nothingPlaying (currentlyPlaying m.fm4SongPlayingInSpotify))
                            :: playlistsPart
                            :: playlistView
                        )
                    ]

        Authenticated _ ->
            div [] [ text "Authenticat" ]

        AuthenticationFailed ->
            div [] [ text "Authentication failed" ]


notFoundView : Html msg
notFoundView =
    div [] [ text "Not Found" ]


comparePlaylists : SpotifyApi.Playlist -> SpotifyApi.Playlist -> Order
comparePlaylists playlist1 playlist2 =
    case ( playlist1.name, playlist2.name ) of
        ( "+", _ ) ->
            LT

        ( _, "+" ) ->
            GT

        ( "o", _ ) ->
            LT

        ( _, "o" ) ->
            GT

        ( "-", _ ) ->
            LT

        ( _, "-" ) ->
            GT

        ( _, _ ) ->
            EQ


sortPlaylists : List SpotifyApi.Playlist -> List SpotifyApi.Playlist
sortPlaylists =
    List.sortWith comparePlaylists


filterPlaylists : List SpotifyApi.Playlist -> List SpotifyApi.Playlist
filterPlaylists =
    List.filter (\p -> (String.length p.name) == 1)


addTracksToPlaylist : String -> List String -> List SpotifyApi.Playlist -> List SpotifyApi.Playlist
addTracksToPlaylist id tracks =
    List.map
        (\p ->
            if p.id == id then
                { p | songs = tracks }
            else
                p
        )


deleteIfMovedTo : SpotifyApi.Playlist -> (SpotifyApi.Playlist -> Bool)
deleteIfMovedTo playlist =
    [ ( "+", \p -> p.name == "-" )
    , ( "o", \p -> (p.name == "-" || p.name == "+") )
    , ( "-", \p -> (p.name == "o" || p.name == "+") )
    ]
        |> Dict.fromList
        |> Dict.get playlist.name
        |> Maybe.withDefault (\_ -> False)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange l ->
            init model l

        CurrentlyPlaying (Ok song) ->
            ( { model | songPlaying = Just song }, Cmd.none )

        CurrentlyPlaying (Err (Http.BadStatus resp)) ->
            if resp.status.code == 401 || resp.status.code == 400 then
                ( model, modifyUrl "/" )
            else
                ( model, Cmd.none )

        CurrentlyPlaying (Err _) ->
            ( model, Cmd.none )

        CurrentlyFm4Playing (Ok song) ->
            ( { model | fm4SongPlaying = song }, song |> Maybe.map (searchTrack model.token) |> Maybe.withDefault Cmd.none )

        CurrentlyFm4Playing (Err err) ->
            ( model, Debug.log (toString err) Cmd.none )

        FoundTracks (Ok song) ->
            ( { model | fm4SongPlayingInSpotify = song }, Cmd.none )

        FoundTracks (Err err) ->
            ( model, Debug.log (toString err) Cmd.none )

        Play ->
            ( model, play model.token )

        PlayPlaylist playlistId ->
            ( model, playPlaylist model.token playlistId model.userId )

        Pause ->
            ( model, pause model.token )

        Next ->
            ( model, next model.token )

        Previous ->
            ( model, prev model.token )

        UpdateCurrentlyPlaying ->
            ( model, fetchCurrentlyPlaying model.token )

        TogglePlay (Ok _) ->
            ( model, after 450 millisecond UpdateCurrentlyPlaying )

        TogglePlay (Err _) ->
            ( model, Cmd.none )

        Me (Ok meId) ->
            ( { model | userId = meId }, Cmd.none )

        Me (Err _) ->
            ( model, Cmd.none )

        PlaylistChange (Ok _) ->
            ( model, fetchPlaylists model.token )

        PlaylistChange (Err _) ->
            ( model, Cmd.none )

        Playlists (Ok p) ->
            let
                newPlaylists =
                    p |> filterPlaylists |> sortPlaylists
            in
                ( { model | playlists = newPlaylists }
                , newPlaylists
                    |> List.map (\p -> fetchPlaylistsTracks model.token (p.href ++ "/tracks?limit=100") p.id)
                    |> Cmd.batch
                )

        Playlists (Err _) ->
            ( model, Cmd.none )

        PlaylistTracks id (Ok tracks) ->
            ( { model | playlists = addTracksToPlaylist id tracks model.playlists }, Cmd.none )

        PlaylistTracks _ (Err _) ->
            ( model, Cmd.none )

        RemoveFromPlaylist playlist song ->
            ( model, removePlaylistTrack model.token playlist.href song.id )

        AddToPlaylist playlist song ->
            let
                addToPlaylist =
                    addPlaylistTrack model.token playlist.href song.id

                deleteFromPlaylists =
                    model.playlists
                        |> List.filter (deleteIfMovedTo playlist)
                        |> List.map (\p -> removePlaylistTrack model.token p.href song.id)
            in
                ( model, Cmd.batch (addToPlaylist :: deleteFromPlaylists) )

        Tick _ ->
            ( model, Cmd.batch [ fetchCurrentlyPlaying model.token, fm4CurrentlyPlaying ] )

        GoToSpotify ->
            ( model, newUrl "/spotify" )

        GoToFm4 ->
            ( model, newUrl "/fm4" )


init : Model -> Location -> ( Model, Cmd Msg )
init model location =
    let
        currentRoute =
            parseLocation location
    in
        case currentRoute of
            Home ->
                ( model, loadToken location.href )

            Authenticated token ->
                ( { model | routes = SpotifyView, token = token }
                , Cmd.batch [ me token, fetchCurrentlyPlaying token, fetchPlaylists token, fm4CurrentlyPlaying, modifyUrl "/spotify" ]
                )

            _ ->
                ( { model | routes = currentRoute }, Cmd.none )


reqWithAuth : String -> String -> String -> Http.Expect a -> Http.Body -> Http.Request a
reqWithAuth token url method expectT body =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = body
        , expect = expectT
        , timeout = Nothing
        , withCredentials = False
        }


loadToken : String -> Cmd msg
loadToken redirectHref =
    load ("https://accounts.spotify.com/authorize?client_id=a83f2dbd775343ebbd1e116aedc0b204&response_type=token&redirect_uri=" ++ redirectHref ++ "authenticated" ++ "&scope=user-read-playback-state user-modify-playback-state playlist-read-private playlist-modify-private playlist-modify-public")


me : String -> Cmd Msg
me token =
    Http.send Me (reqWithAuth token "https://api.spotify.com/v1/me" "GET" (Http.expectJson SpotifyApi.decodeMe) Http.emptyBody)


play : String -> Cmd Msg
play token =
    Http.send TogglePlay (reqWithAuth token "https://api.spotify.com/v1/me/player/play" "PUT" Http.expectString Http.emptyBody)


playPlaylist : String -> String -> String -> Cmd Msg
playPlaylist token id userId =
    let
        body =
            "{\"context_uri\":\"spotify:user:" ++ userId ++ ":playlist:" ++ id ++ "\"}"
    in
        Http.send TogglePlay (reqWithAuth token "https://api.spotify.com/v1/me/player/play" "PUT" Http.expectString (Http.stringBody "application/json" body))


pause : String -> Cmd Msg
pause token =
    Http.send TogglePlay (reqWithAuth token "https://api.spotify.com/v1/me/player/pause" "PUT" Http.expectString Http.emptyBody)


next : String -> Cmd Msg
next token =
    Http.send TogglePlay (reqWithAuth token "https://api.spotify.com/v1/me/player/next" "POST" Http.expectString Http.emptyBody)


prev : String -> Cmd Msg
prev token =
    Http.send TogglePlay (reqWithAuth token "https://api.spotify.com/v1/me/player/previous" "POST" Http.expectString Http.emptyBody)


fetchCurrentlyPlaying : String -> Cmd Msg
fetchCurrentlyPlaying token =
    Http.send CurrentlyPlaying (reqWithAuth token "https://api.spotify.com/v1/me/player/currently-playing" "GET" (Http.expectJson SpotifyApi.decodeSong) Http.emptyBody)


fetchPlaylists : String -> Cmd Msg
fetchPlaylists token =
    Http.send Playlists (reqWithAuth token "https://api.spotify.com/v1/me/playlists" "GET" (Http.expectJson SpotifyApi.decodePlaylists) Http.emptyBody)


fetchPlaylistsTracks : String -> String -> String -> Cmd Msg
fetchPlaylistsTracks token href playlistId =
    paginate href
        .next
        (\url ->
            (reqWithAuth token (url ++ "&fields=items(track(id)),next") "GET" (Http.expectJson SpotifyApi.decodeTrackIds) Http.emptyBody)
        )
        (\newTracks oldTracks -> (newTracks |> .tracks) ++ oldTracks)
        []
        (PlaylistTracks playlistId)


addPlaylistTrack : String -> String -> String -> Cmd Msg
addPlaylistTrack token href songId =
    let
        query =
            "?uris=spotify:track:" ++ songId
    in
        Http.send PlaylistChange (reqWithAuth token (href ++ "/tracks" ++ query) "POST" Http.expectString Http.emptyBody)


searchTrack : String -> FM4Api.Song -> Cmd Msg
searchTrack token song =
    let
        href =
            "https://api.spotify.com/v1"

        query =
            "?q=track:" ++ song.title ++ " artist:" ++ song.interpreter ++ "&type=track"
    in
        Http.send FoundTracks (reqWithAuth token (href ++ "/search" ++ query) "GET" (Http.expectJson SpotifyApi.decodeFoundSong) Http.emptyBody)


removePlaylistTrack : String -> String -> String -> Cmd Msg
removePlaylistTrack token href songId =
    let
        body =
            "{\"tracks\": [{\"uri\": \"spotify:track:" ++ songId ++ "\"}]}"
    in
        Http.send PlaylistChange (reqWithAuth token (href ++ "/tracks") "DELETE" Http.expectString (Http.stringBody "application/json" body))


fm4CurrentlyPlaying : Cmd Msg
fm4CurrentlyPlaying =
    let
        fm4Href =
            "https://audioapi.orf.at/fm4/api/json/current/live"
    in
        Http.send CurrentlyFm4Playing (Http.get fm4Href FM4Api.decodeFM4Responses)
