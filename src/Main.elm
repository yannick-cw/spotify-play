module Main exposing (..)

import Html exposing (div, Html, text, button, img, button, Attribute)
import Html.Attributes exposing (src, disabled, class)
import Html.Events exposing (onClick)
import Navigation exposing (Location, load, newUrl)
import Routing exposing (Route(..), parseLocation)
import Maybe exposing (withDefault)
import Http
import SpotifyApi
import Time exposing (Time, second, millisecond)
import Css
import Markdown
import Delay exposing (after)


type alias Model =
    { routes : Route, songPlaying : Maybe SpotifyApi.Song, playlists : List SpotifyApi.Playlist, userId : String, token : String, l : Location }


type Msg
    = OnLocationChange Location
    | CurrentlyPlaying (Result Http.Error SpotifyApi.Song)
    | Playlists (Result Http.Error (List SpotifyApi.Playlist))
    | PlaylistTracks String (Result Http.Error ( Int, Maybe String, List String ))
    | TogglePlay (Result Http.Error String)
    | Me (Result Http.Error String)
    | PlaylistChange (Result Http.Error String)
    | Play
    | PlayPlaylist String
    | Pause
    | Next
    | Previous
    | AddToPlaylist SpotifyApi.Playlist SpotifyApi.Song
    | UpdateCurrentlyPLaying
    | Tick Time


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = \l -> init (Model Home Nothing [] "" "" l) l
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * second) Tick


view : Model -> Html Msg
view m =
    div [] [ selectRouteView m ]


styles =
    Css.asPairs >> Html.Attributes.style


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs =
    button
        ([ class "btn btn-outline-primary"
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

        Authenticated tk ->
            let
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
                        m.songPlaying

                nothingPlaying =
                    div [] [ text "Nothing is played currently" ]

                playlistsPart =
                    div [] [ text "Playlists:" ]

                highlightIfSongIsPlayingIsIn : SpotifyApi.Playlist -> Maybe SpotifyApi.Song -> List (Attribute Msg)
                highlightIfSongIsPlayingIsIn playlist song =
                    case song of
                        Just s ->
                            if List.any (\id -> id == s.id) playlist.songs then
                                [ styles [ Css.borderColor (Css.rgb 216 2 32) ], disabled True ]
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
                div [] ((Maybe.withDefault nothingPlaying currentlyPlaying) :: playlistsPart :: playlistView)

        AuthenticationFailed ->
            div [] [ text "Authentication failed" ]


notFoundView : Html msg
notFoundView =
    div [] [ text "Not Found" ]


filterPlaylists : List SpotifyApi.Playlist -> List SpotifyApi.Playlist
filterPlaylists =
    List.filter (\p -> (String.length p.name) == 1 || p.name == "TOYA")


addTracksToPlaylist : String -> List String -> Bool -> List SpotifyApi.Playlist -> List SpotifyApi.Playlist
addTracksToPlaylist id tracks overwrite =
    List.map
        (\p ->
            if p.id == id && overwrite then
                { p | songs = tracks }
            else if p.id == id then
                { p | songs = tracks ++ p.songs }
            else
                p
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange l ->
            init model l

        CurrentlyPlaying (Ok song) ->
            ( { model | songPlaying = Just song }, Cmd.none )

        CurrentlyPlaying (Err (Http.BadStatus resp)) ->
            if resp.status.code == 401 then
                ( model, loadToken model.l.href )
            else
                ( model, Cmd.none )

        CurrentlyPlaying (Err _) ->
            ( model, Cmd.none )

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

        UpdateCurrentlyPLaying ->
            ( model, fetchCurrentlyPlaying model.token )

        TogglePlay (Ok _) ->
            ( model, after 500 millisecond UpdateCurrentlyPLaying )

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
                filteredPlaylists =
                    filterPlaylists p
            in
                ( { model | playlists = filteredPlaylists }, filteredPlaylists |> List.map (\p -> fetchPlaylistsTracks model.token (p.href ++ "/tracks?limit=100") p.id) |> Cmd.batch )

        Playlists (Err _) ->
            ( model, Cmd.none )

        PlaylistTracks id (Ok ( offset, Nothing, tracks )) ->
            ( { model | playlists = addTracksToPlaylist id tracks (offset == 0) model.playlists }, Cmd.none )

        PlaylistTracks id (Ok ( offset, Just nextHref, tracks )) ->
            ( { model | playlists = addTracksToPlaylist id tracks (offset == 0) model.playlists }, fetchPlaylistsTracks model.token nextHref id )

        PlaylistTracks _ (Err _) ->
            ( model, Cmd.none )

        AddToPlaylist playlist song ->
            let
                addToPlaylist =
                    addPlaylistTrack model.token playlist.href song.id

                deleteFromOthers =
                    model.playlists |> List.filter (\p -> p.id /= playlist.id) |> List.map (\p -> removePlaylistTrack model.token p.href song.id)
            in
                ( model, Cmd.batch (addToPlaylist :: deleteFromOthers) )

        Tick _ ->
            ( model, fetchCurrentlyPlaying model.token )


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
                ( { model | routes = currentRoute, token = token }, Cmd.batch [ me token, fetchCurrentlyPlaying token, fetchPlaylists token ] )

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
    load ("https://accounts.spotify.com/authorize?client_id=a83f2dbd775343ebbd1e116aedc0b204&response_type=token&redirect_uri=" ++ redirectHref ++ "authenticated" ++ "&scope=user-read-playback-state user-modify-playback-state playlist-read-private playlist-modify-private")


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
    Http.send (PlaylistTracks playlistId) (reqWithAuth token (href ++ "&fields=items(track(id)),next,offset") "GET" (Http.expectJson SpotifyApi.decodeTrackIds) Http.emptyBody)


addPlaylistTrack : String -> String -> String -> Cmd Msg
addPlaylistTrack token href songId =
    let
        query =
            "?uris=spotify:track:" ++ songId
    in
        Http.send PlaylistChange (reqWithAuth token (href ++ "/tracks" ++ query) "POST" Http.expectString Http.emptyBody)


removePlaylistTrack : String -> String -> String -> Cmd Msg
removePlaylistTrack token href songId =
    let
        body =
            "{\"tracks\": [{\"uri\": \"spotify:track:" ++ songId ++ "\"}]}"
    in
        Http.send PlaylistChange (reqWithAuth token (href ++ "/tracks") "DELETE" Http.expectString (Http.stringBody "application/json" body))
