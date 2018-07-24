module ViewHtml exposing (editNodeView, editEdgeView, downloadExportButton, debugCommand)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Tuple exposing (first, second)
import Css exposing (..)
import Types exposing (..)
import Dict exposing (..)
import Physics exposing (distance)
import Http exposing (..)
import ExportDot exposing (exportToDot)


-- Functions which deal only in Html go here.


editNodeView currentViewBox node =
    editElementView currentViewBox node.label (EditNodeMsg node)


editEdgeView currentViewBox edge =
    editElementView currentViewBox edge.label (EditEdgeMsg edge)


editElementView currentViewBox elementLabel onInputCallback =
    let
        curPos =
            ( currentViewBox.x, currentViewBox.y )

        focusPos =
            ( currentViewBox.focusX, currentViewBox.focusY )

        originPos =
            ( currentViewBox.originX, currentViewBox.originY )

        alpha =
            1 - Physics.percentTowardsDest originPos curPos focusPos
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
                , styled input [ borderRadius (px 3) ] [ placeholder elementLabel, onInput onInputCallback ] []
                ]
            ]



-- Export


downloadExportButton model =
    let
        nodes =
            Dict.values model.nodes

        edges =
            Dict.values model.edges

        dot =
            exportToDot nodes edges
    in
        styled a
            []
            [ type_ "button", href <| "data:text/plain;charset=utf-8," ++ encodeUri dot, downloadAs "graph.dot" ]
            [ styled button [] [] [ text "Download" ] ]



--Debug Views


debugCommand =
    Html.Styled.styled div [ position absolute, Css.left (Css.pct 50), Css.fontWeight Css.bold ]


debugFocus =
    Html.Styled.styled div
        [ position absolute
        , Css.left (Css.pct 50)
        , Css.top (Css.pct 50)
        , Css.color (Css.rgba 255 0 0 0.5)
        , Css.fontWeight Css.bold
        ]
