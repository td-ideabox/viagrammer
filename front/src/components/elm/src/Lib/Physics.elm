module Physics exposing (..)

import Dict exposing (Dict)
import Tuple exposing (first, second)


--- Physics calculating functions


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


direction : ( Float, Float ) -> ( Float, Float ) -> Float
direction ( x1, y1 ) ( x2, y2 ) =
    atan2 (y2 - y1) (x2 - x1)


repulse : Float -> Float -> Float -> ( Float, Float )
repulse dist dirTowards radius =
    let
        --- The closer we get the stronger the force.
        f =
            Basics.max ((radius - dist) * 0.005) 0

        dirAway =
            dirTowards - pi

        x =
            (cos dirAway) * f

        y =
            (sin dirAway) * f
    in
        ( x, y )


moveTowards : ( Float, Float ) -> Float -> Float -> Float -> ( Float, Float )
moveTowards ( x, y ) dirTowards speed dt =
    let
        step =
            speed * dt

        ( newX, newY ) =
            ( x + (cos dirTowards) * step
            , y + (sin dirTowards) * step
            )
    in
        ( newX, newY )


attract : Float -> Float -> Float -> ( Float, Float )
attract dist dirTowards radius =
    let
        f =
            Basics.max ((dist - radius) * 0.005) 0

        x =
            (cos dirTowards) * f

        y =
            (sin dirTowards) * f
    in
        ( x, y )


sumForces : List ( Float, Float ) -> ( Float, Float )
sumForces forces =
    List.foldr
        (\( x1, y1 ) ( x2, y2 ) ->
            ( x1 + x2, y1 + y2 )
        )
        ( 0, 0 )
        forces


percentTowardsDest : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
percentTowardsDest originPos currentPos destPos =
    let
        distToDest =
            distance currentPos destPos

        distBetweenOriginAndFocus =
            distance originPos destPos

        normalizedDestDist =
            distToDest / distBetweenOriginAndFocus
    in
        normalizedDestDist
