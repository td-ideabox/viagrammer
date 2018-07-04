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


type alias Coord =
    Float


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


type ArrowHead
    = Pointed


type alias Edge =
    { key : String
    , color : String
    , label : String
    , src : String
    , dest : String
    , arrowHead : ArrowHead
    }


initialEdge : Edge
initialEdge =
    { key = "unassigned edge"
    , color = "#0f0"
    , label = ""
    , src = ""
    , dest = ""
    , arrowHead = Pointed
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
    }


-- Dot Datums from lib
type alias EdgeData = {
    tailIdx: Int,
    headIdx: Int
}

type alias ObjectData = {
    name: String
}

type alias GraphData = {
    edges: List EdgeData,
    objects: List ObjectData
}




-- Errors


type alias Error =
    String
