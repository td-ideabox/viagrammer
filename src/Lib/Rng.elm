module Rng exposing (..)

import Tuple exposing (first, second)


--- Use the fibonnacci sequence as a random number
--- generator


next : ( Int, Int ) -> ( Int, Int )
next rng =
    let
        a =
            first rng

        b =
            second rng

        a2 =
            b

        b2 =
            (a + b) % 1000
    in
        ( a2, b2 )
