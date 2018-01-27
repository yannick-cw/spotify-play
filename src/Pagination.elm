module Pagination exposing (paginate)

import Http exposing (Request, Error, toTask)
import Task exposing (Task)


paginate : next -> (a -> Maybe next) -> (next -> Request a) -> (a -> b -> b) -> b -> (Result Error b -> msg) -> Cmd msg
paginate next resToNext nextToReq combineResults start resultToMsg =
    Task.attempt resultToMsg (rec next resToNext nextToReq combineResults start)


rec : next -> (a -> Maybe next) -> (next -> Request a) -> (a -> b -> b) -> b -> Task Error b
rec next resToNext nextToReq combineResults acc =
    nextToReq next
        |> toTask
        |> Task.andThen
            (\res ->
                case resToNext res of
                    Just n ->
                        rec n resToNext nextToReq combineResults (combineResults res acc)

                    Nothing ->
                        Task.succeed acc
            )
