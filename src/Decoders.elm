module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder, list, tuple3, int, string)

totalsByMonthDecoder : Decoder (List (String, String, Int))
totalsByMonthDecoder =
    list <| (tuple3 (,,) string string int)
