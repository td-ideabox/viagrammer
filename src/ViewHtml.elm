module ViewHtml exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Tuple exposing (first, second)
import Css exposing (..)
import Types exposing (..)
import Dict exposing (..)


-- Functions which deal only in Html go here.


editNodeView : String -> Html Msg
editNodeView nodeIdx =
    Html.Styled.styled
        div
        [ position absolute
        , left (pct 20)
        , top (pct 50)
        , Css.width (px 30)
        , Css.height (px 100)
        , backgroundColor (rgba 30 30 30 0.5)
        , border3 (px 2) solid (rgb 120 120 120)
        ]
        []
        [ label [] [ Html.Styled.text "Label" ]
        , input [ placeholder "Some Label", onInput (Types.EditNodeMsg nodeIdx) ] []
        ]



--Debug Views


debugCommand : List (Attribute msg) -> List (Html msg) -> Html msg
debugCommand =
    Html.Styled.styled div [ position absolute, Css.left (Css.pct 50), Css.fontWeight Css.bold ]


debugFocus : List (Attribute msg) -> List (Html msg) -> Html msg
debugFocus =
    Html.Styled.styled div
        [ position absolute
        , Css.left (Css.pct 50)
        , Css.top (Css.pct 50)
        , Css.color (Css.rgba 255 0 0 0.5)
        , Css.fontWeight Css.bold
        ]
