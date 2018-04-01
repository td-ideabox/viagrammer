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


lineAngle : ( Float, Float ) -> ( Float, Float ) -> Float
lineAngle src dest =
    let
        dX =
            first src - first dest

        dY =
            second src - second dest
    in
        atan2 dY dX
