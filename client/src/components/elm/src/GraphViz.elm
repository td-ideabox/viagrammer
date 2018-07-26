port module GraphViz
    exposing
        ( sendDot
        , layoutData
        , edgeDecoder
        , objectDecoder
        , graphDecoder
        )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Types exposing (..)


edgeDecoder : Decoder EdgeData
edgeDecoder =
    decode EdgeData
        |> required "tail" int
        |> required "head" int


objectDecoder : Decoder ObjectData
objectDecoder =
    decode ObjectData
        |> required "name" string
        |> required "pos" string


graphDecoder : Decoder GraphData
graphDecoder =
    decode GraphData
        |> optional "edges" (list edgeDecoder) []
        |> optional "objects" (list objectDecoder) []


port sendDot : String -> Cmd msg


port layoutData : (String -> msg) -> Sub msg
