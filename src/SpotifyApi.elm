module SpotifyApi
    exposing
        ( decodeSong
        , Song
        , decodePlaylists
        , Playlist
        , decodeTrackIds
        , Me
        , decodeMe
        )

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


-- Song Model


type alias Song =
    { name : String, artist : String, imageUrl : String, id : String }


decodeSong : Json.Decode.Decoder Song
decodeSong =
    Json.Decode.map currentPlayingToSong decodeCurrentlyPlayingResponse


currentPlayingToSong : CurrentlyPlayingResponse -> Song
currentPlayingToSong cp =
    Song cp.item.name (firstNameOr "unknown" cp.item.artists) (selectImage cp.item.album.images) cp.item.id


firstNameOr : String -> List Artist -> String
firstNameOr default artists =
    artists |> List.map .name |> List.head |> Maybe.withDefault default


selectImage : List Image -> String
selectImage images =
    images |> List.reverse |> List.drop 1 |> List.head |> Maybe.map .url |> Maybe.withDefault ""


type alias CurrentlyPlayingResponse =
    { item : SongInfo
    }


type alias Album =
    { images : List Image
    , name : String
    }


type alias SongInfo =
    { album : Album
    , artists : List Artist
    , name : String
    , id : String
    }


type alias Image =
    { url : String }


type alias Artist =
    { name : String }


decodeCurrentlyPlayingResponse : Json.Decode.Decoder CurrentlyPlayingResponse
decodeCurrentlyPlayingResponse =
    Json.Decode.Pipeline.decode CurrentlyPlayingResponse
        |> Json.Decode.Pipeline.required "item" (decodeSongInfo)


decodeAlbum : Json.Decode.Decoder Album
decodeAlbum =
    Json.Decode.Pipeline.decode Album
        |> Json.Decode.Pipeline.required "images" (Json.Decode.list decodeImage)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)


decodeSongInfo : Json.Decode.Decoder SongInfo
decodeSongInfo =
    Json.Decode.Pipeline.decode SongInfo
        |> Json.Decode.Pipeline.required "album" (decodeAlbum)
        |> Json.Decode.Pipeline.required "artists" (Json.Decode.list decodeArtist)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)


decodeImage : Json.Decode.Decoder Image
decodeImage =
    Json.Decode.Pipeline.decode Image
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)


decodeArtist : Json.Decode.Decoder Artist
decodeArtist =
    Json.Decode.Pipeline.decode Artist
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)



-- Playlists Model


type alias PlaylistResponse =
    { items : List Playlist
    }


type alias Playlist =
    { id : String
    , name : String
    , href : String
    , songs : List String
    }


decodePlaylists : Json.Decode.Decoder (List Playlist)
decodePlaylists =
    Json.Decode.map .items decodePlaylistResponse


decodePlaylistResponse : Json.Decode.Decoder PlaylistResponse
decodePlaylistResponse =
    Json.Decode.Pipeline.decode PlaylistResponse
        |> Json.Decode.Pipeline.required "items" (Json.Decode.list decodePlaylist)


decodePlaylist : Json.Decode.Decoder Playlist
decodePlaylist =
    Json.Decode.Pipeline.decode (\id name href -> Playlist id name href [])
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "href" (Json.Decode.string)



-- Playlist Tracks


type alias Tracks =
    { items : List Track
    , next : Maybe String
    , offset : Int
    }


type alias Track =
    { track : TrackId
    }


type alias TrackId =
    { id : String
    }


decodeTrackIds : Json.Decode.Decoder ( Int, Maybe String, List String )
decodeTrackIds =
    Json.Decode.map (\tracks -> ( tracks.offset, tracks.next, tracks.items |> List.map (\t -> t.track.id) )) decodeTracks


decodeTrack : Json.Decode.Decoder Track
decodeTrack =
    Json.Decode.Pipeline.decode Track
        |> Json.Decode.Pipeline.required "track" decodeTrackId


decodeTrackId : Json.Decode.Decoder TrackId
decodeTrackId =
    Json.Decode.Pipeline.decode TrackId
        |> Json.Decode.Pipeline.required "id" Json.Decode.string


decodeTracks : Json.Decode.Decoder Tracks
decodeTracks =
    Json.Decode.Pipeline.decode Tracks
        |> Json.Decode.Pipeline.required "items" (Json.Decode.list decodeTrack)
        |> Json.Decode.Pipeline.required "next" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "offset" (Json.Decode.int)



-- me


type alias Me =
    { id : String }


decodeMe : Json.Decode.Decoder String
decodeMe =
    Json.Decode.Pipeline.decode Me
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.map .id
