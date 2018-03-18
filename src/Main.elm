module Main exposing (..)

import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Keyboard exposing (..)
import Physics exposing (attract, direction, distance, moveTowards, repulse, sumForces)
import String exposing (toInt)
import Task
import Time exposing (Time)
import Window
import Html
import Html.Styled exposing (..)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Rng exposing (next)
import View exposing (..)
import State exposing (..)
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { view = View.view >> Html.Styled.toUnstyled
        , init = State.init
        , update = State.update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs Frame
                    , Window.resizes WindowSize
                    ]
        }
