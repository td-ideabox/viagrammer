module Rng exposing (..)

--- Use the fibonnacci sequence as a random number
--- generator


next ( a, b ) =
    ( b, (a + b) % 1000 )
