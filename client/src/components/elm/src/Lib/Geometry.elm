module Geometry exposing (..)


lineMidPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
lineMidPoint ( srcX, srcY ) ( destX, destY ) =
    ( (srcX + destX) / 2
    , (srcY + destY) / 2
    )


lineAngle : ( Float, Float ) -> ( Float, Float ) -> Float
lineAngle ( srcX, srcY ) ( destX, destY ) =
    atan2 (srcY - destY) (srcX - destX)
