module Main exposing (..)

import Navigation exposing (Location, load, newUrl, modifyUrl)
import Routing exposing (Route(..), parseLocation)
import Maybe exposing (withDefault)
import Http
import SpotifyApi
import Pagination exposing (paginate)
import Delay exposing (after)
import Time exposing (Time, second, millisecond)
import Dict
import FM4Api exposing (lastPlayingSong)
import View exposing (view)
import Model exposing (Model, Msg(..))


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

        CurrentlyFm4Playing (Err (Http.BadStatus resp)) ->
            if resp.status.code == 401 || resp.status.code == 400 then
                ( model, modifyUrl "/" )
            else
                ( model, Cmd.none )

        CurrentlyFm4Playing (Err _) ->
            ( model, Cmd.none )

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
            ( model
            , if model.routes == SpotifyView then
                fetchCurrentlyPlaying model.token
              else if model.routes == Fm4View then
                fm4CurrentlyPlaying
              else
                Cmd.none
            )

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
