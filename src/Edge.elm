module Edge exposing (..)

import Dict exposing (Dict)
import Node exposing (Node)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)


type alias Edge =
    { key : String
    , color : String
    , label : String
    , src : String
    , dest : String
    }


initialEdge : Edge
initialEdge =
    { key = "unassigned edge"
    , color = "#0f0"
    , label = ""
    , src = ""
    , dest = ""
    }


edgeToSvg : Edge -> Dict String Node -> Svg msg
edgeToSvg edge nodes =
    let
        src =
            Dict.get edge.src nodes

        srcX =
            case src of
                Just s ->
                    toString s.x

                Nothing ->
                    Debug.crash "Cant render edge svg cause src x node missing from node map"

        srcY =
            case src of
                Just s ->
                    toString s.y

                Nothing ->
                    Debug.crash "Cant render src y of edge svg cause node missing from node map"

        dest =
            Dict.get edge.dest nodes

        destX =
            case dest of
                Just d ->
                    toString d.x

                Nothing ->
                    Debug.crash "Cant render edge cause dest x is missing from node map"

        destY =
            case dest of
                Just d ->
                    toString d.y

                Nothing ->
                    Debug.crash "Cant render edge cause desy y is missing from node map"
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
            ]
