module Types exposing (..)

import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Keyboard exposing (..)
import Task
import Time exposing (Time)
import Window
import Json.Decode exposing (..)


type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size
    | EditNodeMsg Node String
    | EditEdgeMsg Edge String
    | UpdateLayout String


type Key
    = Return
    | Escape
    | Elwr
    | Ilwr
    | Nlwr
    | Other


getKey : Keyboard.KeyCode -> Key
getKey keyCode =
    case keyCode of
        13 ->
            Return

        27 ->
            Escape

        69 ->
            Elwr

        73 ->
            Ilwr

        78 ->
            Nlwr

        _ ->
            Other


type Mode
    = Normal
    | EditNode Node
    | EditEdge Edge



---- MODEL ----


type alias XCoord =
    Float


type alias YCoord =
    Float


type alias Position =
    ( XCoord, YCoord )


type alias Width =
    Inches


type alias Height =
    Inches


type alias Diminsions =
    ( Width, Height )


type alias ViewBox =
    { x : Float
    , y : Float
    , zoom : Float
    , focusX : Float
    , focusY : Float
    , originX : Float
    , originY : Float
    }


initialViewBox : ViewBox
initialViewBox =
    { x = 0
    , y = 0
    , zoom = 1
    , focusX = 0
    , focusY = 0
    , originX = 0
    , originY = 0
    }


type alias NodeIdx =
    String


type alias NodeColor =
    String


type alias NodeLabel =
    String


type alias NodeStyling =
    String


type alias Inches =
    Float


type alias NumPixels =
    Int


type alias AnchorCoord =
    Maybe Position


type alias Node =
    { idx : NodeIdx
    , color : NodeColor
    , position : Position
    , anchorCoord : AnchorCoord
    , ignoreForces : Bool
    , diminsions : Diminsions
    , label : NodeLabel
    }


newNode : NodeIdx -> NodeColor -> Position -> AnchorCoord -> Bool -> Diminsions -> NodeLabel -> Node
newNode nodeIdx nodeColor position anchorCoord ignoreForces diminsions nodeLabel =
    { idx = nodeIdx
    , color = nodeColor
    , position = position
    , anchorCoord = anchorCoord
    , ignoreForces = ignoreForces
    , diminsions = diminsions
    , label = nodeLabel
    }



-- http://package.elm-lang.org/packages/elm-community/result-extra/2.2.0/Result-Extra


combineResult : List (Result x a) -> Result x (List a)
combineResult =
    List.foldr (Result.map2 (::)) (Ok [])


updateNodeWithObjectData : ObjectData -> Node -> Node
updateNodeWithObjectData objectData node =
    let
        coordsResults =
            String.split "," objectData.pos
                |> List.map (\coordStr -> String.toFloat coordStr)
                |> combineResult
    in
        case coordsResults of
            Ok coordRes ->
                case coordRes of
                    [ x, y ] ->
                        { node | anchorCoord = Just ( x, y ) }

                    _ ->
                        Debug.crash ("Failed to parse " ++ objectData.pos)

            Err err ->
                Debug.crash (err)


setNodeAnchor : ( Float, Float ) -> Node -> Node
setNodeAnchor anchor node =
    { node | anchorCoord = Just anchor }


type ArrowHead
    = Pointed


type alias EdgeKey =
    String


type alias EdgeColor =
    String


type alias EdgeLabel =
    String


type alias EdgeSrc =
    String


type alias EdgeDest =
    String


type alias Edge =
    { key : EdgeKey
    , color : EdgeColor
    , label : EdgeLabel
    , src : EdgeSrc
    , dest : EdgeDest
    , arrowHead : ArrowHead
    }


newEdge : EdgeKey -> EdgeColor -> EdgeLabel -> EdgeSrc -> EdgeDest -> ArrowHead -> Edge
newEdge edgeKey edgeColor edgeLabel edgeSrc edgeDest arrowHead =
    { key = edgeKey
    , color = edgeColor
    , label = edgeLabel
    , src = edgeSrc
    , dest = edgeDest
    , arrowHead = arrowHead
    }


type alias Model =
    { nodes : Dict String Node
    , edges : Dict String Edge
    , indexAlphabet : Array String
    , commandAlphabet : List Char
    , indexCounter : Int
    , mode : Mode
    , errMsg : Error
    , currentCommand : String
    , windowSize : Window.Size
    , viewBox : ViewBox
    , rng : ( Int, Int )
    , layoutData : Maybe GraphData
    }


model : Model
model =
    { nodes = Dict.empty
    , edges = Dict.empty
    , mode = Normal
    , indexAlphabet = Array.fromList [ "a", "b", "c", "f", "g", "h", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "u", "v", "w", "x", "y", "z" ]
    , commandAlphabet = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ]
    , indexCounter = 0
    , errMsg = ""
    , currentCommand = ""
    , windowSize = { width = 0, height = 0 }
    , viewBox = initialViewBox
    , rng = ( 1, 1 ) -- Use fib sequence to generate rng values
    , layoutData = Nothing
    }


type alias EdgeData =
    { tail : Int
    , head : Int
    }


type alias ObjectData =
    { name : String
    , pos : String
    }


type alias GraphData =
    { edges : List EdgeData
    , objects : List ObjectData
    }



-- Errors


type alias Error =
    String
