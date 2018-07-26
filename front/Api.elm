module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type NoContent = NoContent

type alias Item =
    { id : Int
    , text : String
    }

decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "id" int
        |> required "text" string

getApiItem : Http.Request (List (Int))
getApiItem =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiItemByItemId : Int -> Http.Request (Item)
getApiItemByItemId capture_itemId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeItem
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiItem : String -> Http.Request (Int)
postApiItem body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteApiItemByItemId : Int -> Http.Request (NoContent)
deleteApiItemByItemId capture_itemId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }