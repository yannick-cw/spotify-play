module FM4Api exposing (decodeFM4Responses, lastPlayingSong, Song)

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


type alias Song =
    { title : String, interpreter : String }


type alias Item =
    { isCompleted : Bool
    , end : Int
    , state : String
    , title : Maybe String
    , interpreter : Maybe String
    }


type alias FM4Response =
    { items : List Item }


type alias FM4Responses =
    List FM4Response


decodeItem : Json.Decode.Decoder Item
decodeItem =
    Json.Decode.Pipeline.decode Item
        |> Json.Decode.Pipeline.required "isCompleted" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "end" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "state" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "title" (Json.Decode.maybe Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "interpreter" (Json.Decode.maybe Json.Decode.string) Nothing


decodeFM4Response : Json.Decode.Decoder FM4Response
decodeFM4Response =
    Json.Decode.Pipeline.decode FM4Response
        |> Json.Decode.Pipeline.required "items" (Json.Decode.list decodeItem)


decodeFM4Responses : Json.Decode.Decoder (Maybe Song)
decodeFM4Responses =
    (Json.Decode.list decodeFM4Response) |> Json.Decode.map lastPlayingSong


lastPlayingSong : FM4Responses -> Maybe Song
lastPlayingSong response =
    response
        |> List.concatMap .items
        |> List.sortBy .end
        |> List.filter (\item -> item.isCompleted == False)
        |> List.filterMap
            (\item -> Maybe.map2 Song item.title item.interpreter)
        |> List.head
