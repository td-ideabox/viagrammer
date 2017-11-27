module Input exposing (..)

import Keyboard


type Key
    = LeftArrow
    | RightArrow
    | UpArrow
    | DownArrow
    | WLwr
    | ALwr
    | SLwr
    | DLwr
    | Escape
    | Elwr
    | Nlwr
    | SpaceBar
    | Other


getKey : Keyboard.KeyCode -> Key
getKey keyCode =
    case keyCode of
        27 ->
            Escape

        32 ->
            SpaceBar

        37 ->
            LeftArrow

        38 ->
            UpArrow

        39 ->
            RightArrow

        40 ->
            DownArrow

        65 ->
            ALwr

        68 ->
            DLwr

        69 ->
            Elwr

        78 ->
            Nlwr

        83 ->
            SLwr

        87 ->
            WLwr

        _ ->
            Other
