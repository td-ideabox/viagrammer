module Geometry exposing (..)

import Tuple exposing (first, second)


lineMidPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
lineMidPoint src dest =
    let
        x =
            (first src + first dest) / 2

        y =
            (second src + second dest) / 2
    in
        ( x, y )
