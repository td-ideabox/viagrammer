module DebugView exposing (..)

import Html
import Html.Styled exposing (..)
import Tuple exposing (first, second)
import Css exposing (absolute, px, em, position, left, bold)


debugCommand : List (Attribute msg) -> List (Html msg) -> Html msg
debugCommand =
    styled div [ position absolute, Css.left (Css.pct 50), Css.fontWeight Css.bold ]


debugFocus : List (Attribute msg) -> List (Html msg) -> Html msg
debugFocus =
    styled div
        [ position absolute
        , Css.left (Css.pct 50)
        , Css.top (Css.pct 50)
        , Css.color (Css.rgb 255 0 0)
        , Css.fontWeight Css.bold
        ]
