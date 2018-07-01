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


-- Functions which deal only in Html go here.


editNodeView : ViewBox -> Node -> Html Msg
editNodeView currentViewBox node =
    editElementView currentViewBox node.label (EditNodeMsg node)


editEdgeView : ViewBox -> Edge -> Html Msg
editEdgeView currentViewBox edge =
    editElementView currentViewBox edge.label (EditEdgeMsg edge)


editElementView : ViewBox -> String -> (String -> Msg) -> Html Msg
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


downloadExportButton : Model -> Html Msg
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


exportToDot : List Node -> List Edge -> String
exportToDot nodes edges =
    let
        nodeStr =
            nodesToDot nodes

        edgeStr =
            edgesToDot edges

        content =
            String.join "\n" [ nodeStr, edgeStr ]

        dot =
            String.concat [ "digraph SomeGraph ", "{\n", content, "\n}" ]
    in
        dot


edgesToDot : List Edge -> String
edgesToDot edges =
    List.map
        (\edge ->
            case edge.label of
                "" ->
                    { edge | label = edge.key }

                _ ->
                    edge
        )
        edges
        |> List.map
            (\edge ->
                String.concat [ "\t", edge.src, "->", edge.dest, "[label=\"", edge.label, "\"]" ]
            )
        |> String.join "\n"


nodesToDot : List Node -> String
nodesToDot nodes =
    List.map
        (\node ->
            let
                label =
                    node.label
            in
                case label of
                    "" ->
                        String.concat [ node.idx ]

                    _ ->
                        String.concat [ node.idx, "[label=\"", node.label, "\"]" ]
        )
        nodes
        |> List.map (\str -> "\t" ++ str)
        |> String.join "\n"



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
