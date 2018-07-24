module Geometry exposing (..)


lineMidPoint ( srcX, srcY ) ( destX, destY ) =
    ( (srcX + destX) / 2
    , (srcY + destY) / 2
    )


lineAngle ( srcX, srcY ) ( destX, destY ) =
    atan2 (srcY - destY) (srcX - destX)
