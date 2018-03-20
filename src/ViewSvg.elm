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
                            in
                                g []
                                    [ line
                                        [ x1 srcX
                                        , y1 srcY
                                        , x2 destX
                                        , y2 destY
                                        , stroke "black"
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
