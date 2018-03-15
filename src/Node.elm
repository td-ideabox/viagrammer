module Node exposing (..)

import Array exposing (Array)
import Dict
import Svg
import Svg.Styled exposing (Svg, g, line, rect, svg, text_)
import Svg.Styled.Attributes exposing (width, height, viewBox, x, y, x1, y1, x2, y2, stroke, rx, ry, fill)
import Tuple exposing (first, second)


type alias Node =
    { idx : String
    , color : String
    , x : Float
    , y : Float
    , ignoreForces : Bool
    , width : Int
    , height : Int
    , roundX : Int
    , roundY : Int
    , label : String
    }



-- We're initializing data with valid but incorrect data. Is
-- there a better pattern?


initialNode : Node
initialNode =
    { idx = "unassigned"
    , x = 0
    , y = 0
    , ignoreForces = False
    , label = ""
    , color = "#f00"
    , width = 80
    , height = 80
    , roundX = 15
    , roundY = 15
    }


findNodeByIdx : List Node -> String -> Maybe Node
findNodeByIdx nodes idx =
    List.filter (\n -> n.idx == idx) nodes |> List.head


buildIdx : Int -> Array String -> String
buildIdx numNodes alphabet =
    let
        alphaLength =
            Array.length alphabet

        --- Integer division to find quotient (how long the id is going to be if
        --- there are more nodes than characters in the alphabet)
        q =
            numNodes // alphaLength

        i =
            numNodes % alphaLength

        a =
            Array.get i alphabet
    in
        case a of
            Nothing ->
                --- TODO: Handle the case where we could not get a letter from
                --- the alphabet
                Debug.crash "Could not get char from alphabet while building idx!"

            Just val ->
                if q == 0 then
                    val
                else
                    val ++ buildIdx (numNodes - alphaLength) alphabet


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
            toString (round node.x) ++ ", " ++ toString (round node.y)

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
