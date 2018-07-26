module ViewSvg exposing (..)

import Tuple exposing (first, second)
import Types exposing (..)
import Dict exposing (..)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Basics exposing (round)
import Geometry exposing (..)
import Physics exposing (distance)
import ExportDot exposing (..)


-- Functions which deal only with svg go here
-- Edge Views


svgCanvas : Model -> Svg msg
svgCanvas model =
    let
        ( winWidth, winHeight ) =
            ( toFloat model.windowSize.width
            , toFloat model.windowSize.height
            )

        zoom =
            model.viewBox.zoom

        ( viewWidth, viewHeight ) =
            ( winWidth * zoom |> toString
            , winHeight * zoom |> toString
            )

        nodesSvg =
            Dict.map (\k v -> nodeToSvg v) model.nodes |> Dict.values

        edgesSvg =
            Dict.map (\k v -> edgeToSvg v model.nodes) model.edges |> Dict.values

        elements =
            List.append nodesSvg edgesSvg

        ( viewX, viewY ) =
            ( model.viewBox.x |> toString
            , model.viewBox.y |> toString
            )

        viewBoxAttr =
            String.join " " [ viewX, viewY, viewWidth, viewHeight ]
    in
        svg [ width viewWidth, height viewHeight, viewBox viewBoxAttr ] elements


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
                                ( srcConnectPoints, destConnectPoints ) =
                                    ( calcConnectPoints s
                                    , calcConnectPoints d
                                    )

                                connectionPoints =
                                    findClosestNeighborPoints srcConnectPoints destConnectPoints

                                ( srcCPoint, destCPoint ) =
                                    connectionPoints

                                ( sCX, sCY ) =
                                    srcCPoint

                                ( dCX, dCY ) =
                                    destCPoint

                                ( srcX, srcY ) =
                                    ( toString sCX, toString sCY )

                                ( destX, destY ) =
                                    ( toString dCX, toString dCY )

                                ( midX, midY ) =
                                    Geometry.lineMidPoint ( sCX, sCY ) ( dCX, dCY )

                                ( labelX, labelY ) =
                                    ( toString midX, toString midY )

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


calcConnectPoints : Node -> List ( Float, Float )
calcConnectPoints node =
    let
        ( x, y ) =
            node.position

        convertToPixels =
            \a -> inchesToPixels a |> toFloat

        ( width, height ) =
            node.diminsions

        ( widthPx, heightPx ) =
            ( convertToPixels width, convertToPixels height )

        halfWidth =
            widthPx / 2.0

        halfHeight =
            heightPx / 2.0

        midTop =
            ( x + halfWidth, y )

        midBottom =
            ( x + halfWidth, y + heightPx )

        midLeft =
            ( x, y + halfHeight )

        midRight =
            ( x + widthPx, y + halfHeight )
    in
        [ midTop, midBottom, midLeft, midRight ]



{-
   @TODO Handle case where no minimum exists
   Given a list of src points and destination points:
   1. Create a list of all possible pairs by mapping over both lists and concating the results together
   2. Calculate the distance between those points
   3. Sort the results by that distance, minimum first
   4. Return that pair of closest points
-}


findClosestNeighborPoints : List ( Float, Float ) -> List ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) )
findClosestNeighborPoints srcPoints destPoints =
    let
        allPairs =
            List.map (\s -> List.map (\d -> ( s, d )) destPoints) srcPoints |> List.concat

        closestPair =
            List.map
                (\( coord1, coord2 ) ->
                    ( Physics.distance coord1 coord2, ( coord1, coord2 ) )
                )
                allPairs
                |> List.sortBy (\x -> first x)
                |> List.head
    in
        case closestPair of
            Just p ->
                second p

            Nothing ->
                Debug.crash "No closest pair?"



-- Node Views


nodeToSvg : Node -> Svg msg
nodeToSvg node =
    let
        ( xPos, yPos ) =
            node.position

        ( xStr, yStr ) =
            ( toString xPos, toString yPos )

        ( nWidthInches, nHeightInches ) =
            node.diminsions

        ( nWidthPixels, nHeightPixels ) =
            ( inchesToPixels nWidthInches, inchesToPixels nHeightInches )

        ( idxX, idxY ) =
            ( xPos + 10 |> toString, yPos + 15 |> toString )

        labelX =
            toFloat nWidthPixels
                |> (*) 0.25
                |> (+) (Tuple.first node.position)
                |> toString

        labelY =
            toFloat nHeightPixels
                |> (*) 0.5
                |> (+) (Tuple.second node.position)
                |> toString
    in
        g []
            [ rect
                [ x xStr
                , y yStr
                , toString nWidthPixels
                    |> width
                , toString nHeightPixels
                    |> height
                , fill "transparent"
                , stroke "black"
                ]
                []
            , text_ [ x idxX, y idxY ] [ Svg.Styled.text node.idx ]
            , text_ [ x labelX, y labelY ] [ Svg.Styled.text node.label ]
            ]
