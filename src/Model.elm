module Model exposing (Model, Msg(..))

import SpotifyApi
import FM4Api exposing (lastPlayingSong)
import Navigation exposing (Location, load, newUrl, modifyUrl)
import Http
import Time exposing (Time)
import Routing exposing (Route)


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
