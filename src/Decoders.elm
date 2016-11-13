module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder, tuple2, list, tuple3, int, string)

totalsByMonthDecoder : Decoder (List (String, String, Int))
totalsByMonthDecoder =
    list <| (tuple3 (,,) string string int)


mostStarredReposDecoder : Decoder (List (String, String, Int))
mostStarredReposDecoder =
    list <| (tuple3 (,,) string string int)


mostReposCreatedDecoder : Decoder (List (String, Int))
mostReposCreatedDecoder =
    list <| (tuple2 (,) string int)

mostStarsForReposDecoder : Decoder (List (String, Int))
mostStarsForReposDecoder =
    list <| (tuple2 (,) string int)
