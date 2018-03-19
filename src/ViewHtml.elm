module ViewHtml exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Tuple exposing (first, second)
import Css exposing (..)
import Types exposing (..)
import Dict exposing (..)
import Physics exposing (distance)


-- Functions which deal only in Html go here.


editNodeView : ViewBox -> Node -> Html Msg
editNodeView currentViewBox node =
    let
        curPos =
            ( currentViewBox.x, currentViewBox.y )

        focusPos =
            ( currentViewBox.focusX, currentViewBox.focusY )

        distToDest =
            distance curPos focusPos

        originPos =
            ( currentViewBox.originX, currentViewBox.originY )

        distFromOrigin =
            distance curPos originPos

        distBetweenOriginAndFocus =
            distance originPos focusPos

        normalizedDestDist =
            distToDest / distBetweenOriginAndFocus

        normalizedOriginDist =
            distFromOrigin / distBetweenOriginAndFocus

        alpha =
            normalizedOriginDist - normalizedDestDist
    in
        Html.Styled.styled
            div
            [ position absolute
            , left (pct 30)
            , top (pct 50)
            , Css.width (px 200)
            , Css.height (px 100)
            , opacity (Css.num alpha)
            , backgroundColor (rgba 255 255 230 1)
            , border3 (px 2) solid (rgb 230 210 200)
            , borderRadius (px 4)
            ]
            []
            [ styled div
                [ position relative
                , padding (px 8)
                ]
                []
                [ styled label [ marginRight (px 2) ] [] [ Html.Styled.text "Label" ]
                , styled input [ borderRadius (px 3) ] [ placeholder node.label, onInput (Types.EditNodeMsg node) ] []
                ]
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
