module Main exposing (..)

import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Task
import Time exposing (Time)
import Window
import Html
import Html.Styled exposing (..)
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
