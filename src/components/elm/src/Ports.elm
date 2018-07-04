port module Ports exposing (sendDot, layoutData)

port sendDot: String -> Cmd msg

port layoutData: (String -> msg) -> Sub msg