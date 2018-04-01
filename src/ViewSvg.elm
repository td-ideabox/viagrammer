module ViewSvg exposing (..)

import Tuple exposing (first, second)
import Types exposing (..)
import Dict exposing (..)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Basics exposing (round)
import Geometry exposing (..)


-- Functions which deal only with svg go here
-- Edge Views


svgCanvas : Model -> Svg msg
svgCanvas model =
    let
        winWidth =
            toFloat model.windowSize.width

        winHeight =
            toFloat model.windowSize.height

        zoom =
            model.viewBox.zoom

        viewWidth =
            winWidth * zoom |> toString

        viewHeight =
            winHeight * zoom |> toString

        nodesSvg =
            Dict.map (\k v -> nodeToSvg v) model.nodes |> Dict.values

        edgesSvg =
            Dict.map (\k v -> edgeToSvg v model.nodes) model.edges |> Dict.values

        elements =
            List.append nodesSvg edgesSvg

        viewX =
            model.viewBox.x |> toString

        viewY =
            model.viewBox.y |> toString

        viewBoxAttr =
            String.join " " [ viewX, viewY, viewWidth, viewHeight ]
    in
        svg [ width viewWidth, height viewHeight, viewBox viewBoxAttr ] elements


arrowHeadToSvg : ( Float, Float ) -> ( Float, Float ) -> ArrowHead -> Svg msg
arrowHeadToSvg src dest headKind =
    case headKind of
        Pointed ->
            let
                sX =
                    first src

                sY =
                    second src

                dX =
                    first dest

                dY =
                    second dest

                length =
                    10

                rotationDeg =
                    30.0

                lineAngle =
                    Geometry.lineAngle ( sX, sY ) ( dX, dY )

                l1 =
                    lineAngle + rotationDeg

                l2 =
                    lineAngle - rotationDeg

                endX =
                    cos lineAngle
                        |> degrees
                        |> (+) rotationDeg
                        |> (*) (dX - length)

                endY =
                    cos lineAngle
                        |> degrees
                        |> (-) rotationDeg
                        |> (*) (dY - length)

                rotationXCoord =
                    dX

                rotationYCoord =
                    dY

                rotationAttr =
                    String.join " " [ toString lineAngle, toString rotationXCoord, toString rotationYCoord ]
                        |> rotate
            in
                g []
                    [ line
                        [ toString dX |> x1
                        , toString dY |> y1
                        , toString endX |> x2
                        , toString endY |> y2
                        , stroke "black"

                        --     , rotationAttr
                        ]
                        []
                    ]


markerSvg : String -> String -> Svg msg
markerSvg startId endId =
    defs
        []
        [ marker [ id startId, markerWidth "8", markerHeight "8", refX "5", refY "5" ]
            [ circle [ cx "5", cy "5", r "3", Svg.Styled.Attributes.style "stroke: none; fill: #000000;" ] []
            ]
        , marker [ id endId, markerWidth "13", markerHeight "13", refX "2", refY "6", orient "auto" ]
            [ Svg.Styled.path [ d "M2,2 L2,11 L10,6 L2,2", Svg.Styled.Attributes.style "fill: #000000;" ] []
            ]
        ]


edgeToSvg : Edge -> Dict String Node -> Svg msg
edgeToSvg edge nodes =
    let
        src =
            Dict.get edge.src nodes
    in
        case src of
            Just s ->
                let
                    dest =
                        Dict.get edge.dest nodes
                in
                    case dest of
                        Just d ->
                            let
                                srcX =
                                    toString s.x

                                srcY =
                                    toString s.y

                                destX =
                                    toString d.x

                                destY =
                                    toString d.y

                                midPoint =
                                    Geometry.lineMidPoint ( s.x, s.y ) ( d.x, d.y )

                                labelX =
                                    first midPoint |> toString

                                labelY =
                                    second midPoint |> toString

                                startId =
                                    String.concat [ "start", edge.key ]

                                endId =
                                    String.concat [ "end", edge.key ]

                                markers =
                                    markerSvg startId endId
                            in
                                g []
                                    [ markers
                                    , line
                                        [ x1 srcX
                                        , y1 srcY
                                        , x2 destX
                                        , y2 destY
                                        , stroke "black"
                                        , Svg.Styled.Attributes.style <| String.concat [ "marker-start: url(#", startId, ");", "marker-end: url(#", endId, ");" ]
                                        ]
                                        []
                                    , text_ [ x labelX, y labelY ] [ Svg.Styled.text edge.label ]
                                    ]

                        Nothing ->
                            Debug.crash "Cant render edge because dest node is missing: "

            Nothing ->
                Debug.crash "Cant render edge svg cause src node is missing"



-- Node Views


nodeToSvg : Node -> Svg msg
nodeToSvg node =
    let
        xPos =
            toString node.x

        yPos =
            toString node.y

        nWidth =
            toString node.width

        nHeight =
            toString node.height

        roundX =
            toString node.roundX

        roundY =
            toString node.roundY

        idx =
            node.idx

        idxX =
            toString (node.x + 10)

        idxY =
            toString (node.y + 15)

        debugCoordX =
            toString (node.x + 10)

        debugCoordY =
            toString (node.y + 60)

        debugCoord =
            toString (Basics.round node.x) ++ ", " ++ toString (round node.y)

        labelX =
            toFloat node.width
                |> (*) 0.25
                |> (+) node.x
                |> toString

        labelY =
            toFloat node.height
                |> (*) 0.5
                |> (+) node.y
                |> toString
    in
        g []
            [ rect
                [ x xPos
                , y yPos
                , width nWidth
                , height nHeight
                , rx roundX
                , ry roundY
                , fill "transparent"
                , stroke "black"
                ]
                []
            , text_ [ x idxX, y idxY ] [ Svg.Styled.text node.idx ]
            , text_ [ x debugCoordX, y debugCoordY ] [ Svg.Styled.text debugCoord ]
            , text_ [ x labelX, y labelY ] [ Svg.Styled.text node.label ]
            ]
