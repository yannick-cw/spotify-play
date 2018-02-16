module SpotifyApi
    exposing
        ( decodeSong
        , Song
        , decodePlaylists
        , decodeFoundSong
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



-- SearchRes model


type alias Songs =
    { songs : List SongInfo }


decodeFoundSong : Json.Decode.Decoder (Maybe Song)
decodeFoundSong =
    decodeSearchRes
        |> Json.Decode.map
            (\songs ->
                songs.songs
                    |> List.head
                    |> Maybe.map CurrentlyPlayingResponse
                    |> Maybe.map currentPlayingToSong
            )


decodeSearchRes : Json.Decode.Decoder Songs
decodeSearchRes =
    Json.Decode.Pipeline.decode Songs
        |> Json.Decode.Pipeline.requiredAt [ "tracks", "items" ] (Json.Decode.list decodeSongInfo)



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
--{
--"tracks": {
--"href": "https://api.spotify.com/v1/search?query=track%3APray+For+Me+artist%3AKendrick+Lamar+&type=track&market=DE&offset=0&limit=20",
--"items": [
--{
--"album": {
--"album_type": "single",
--"artists": [
--{
--"external_urls": {
--"spotify": "https://open.spotify.com/artist/1Xyo4u8uXC1ZmMpatF05PJ"
--},
--"href": "https://api.spotify.com/v1/artists/1Xyo4u8uXC1ZmMpatF05PJ",
--"id": "1Xyo4u8uXC1ZmMpatF05PJ",
--"name": "The Weeknd",
--"type": "artist",
--"uri": "spotify:artist:1Xyo4u8uXC1ZmMpatF05PJ"
--},
--{
--"external_urls": {
--"spotify": "https://open.spotify.com/artist/2YZyLoL8N0Wb9xBt1NhZWg"
--},
--"href": "https://api.spotify.com/v1/artists/2YZyLoL8N0Wb9xBt1NhZWg",
--"id": "2YZyLoL8N0Wb9xBt1NhZWg",
--"name": "Kendrick Lamar",
--"type": "artist",
--"uri": "spotify:artist:2YZyLoL8N0Wb9xBt1NhZWg"
--}



--],
--"available_markets": [],
--"external_urls": {
--"spotify": "https://open.spotify.com/album/1mESN9Zy0787YRMuLqOY4j"
--},
--"href": "https://api.spotify.com/v1/albums/1mESN9Zy0787YRMuLqOY4j",
--"id": "1mESN9Zy0787YRMuLqOY4j",
--"images": [
--{
--"height": 640,
--"url": "https://i.scdn.co/image/c6d179eef98885a3e2f9c50fb61518a3b9274d4b",
--"width": 640
--},
--{
--"height": 300,
--"url": "https://i.scdn.co/image/779780d9880e529ab723041f7d98aa585809e2fe",
--"width": 300
--},
--{
--"height": 64,
--"url": "https://i.scdn.co/image/a8aa7dde9d982322af36a141c0431ac105f25ec1",
--"width": 64
--}



--],
--"name": "Pray For Me (with Kendrick Lamar)",
--"type": "album",
--"uri": "spotify:album:1mESN9Zy0787YRMuLqOY4j"
--},


type alias Tracks =
    { items : List Track
    , next : Maybe String
    }


type alias Track =
    { track : TrackId
    }


type alias TrackId =
    { id : String
    }


type alias PaginationRes =
    { next : Maybe String
    , tracks : List String
    }


decodeTrackIds : Json.Decode.Decoder PaginationRes
decodeTrackIds =
    Json.Decode.map (\tracks -> (PaginationRes tracks.next (tracks.items |> List.map (\t -> t.track.id)))) decodeTracks


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



-- me


type alias Me =
    { id : String }


decodeMe : Json.Decode.Decoder String
decodeMe =
    Json.Decode.Pipeline.decode Me
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.map .id
