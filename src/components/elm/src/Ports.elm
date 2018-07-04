port module Ports exposing (sendDot)

port sendDot: String -> Cmd msg

port receiveLayoutData: (String -> msg) -> Sub msg