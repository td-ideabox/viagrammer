module Main exposing (..)

import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Edge exposing (Edge, edgeToSvg, initialEdge)
import Keyboard exposing (..)
import Physics exposing (attract, direction, distance, moveTowards, repulse, sumForces)
import String exposing (toInt)
import Task
import Time exposing (Time)
import Tuple exposing (..)
import Window
import Window
import Html
import Html.Styled exposing (..)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Rng exposing (next)
import Graph exposing (..)
import DebugView exposing (..)
import Node exposing (Node, buildIdx, findNodeByIdx, initialNode, nodeToSvg)


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
    | LabelNode
    | LabelEdge



---- MODEL ----


type alias Coord =
    Float


type alias ViewBox =
    { x : Float
    , y : Float
    , zoom : Float
    , focusX : Float
    , focusY : Float
    }


initialViewBox : ViewBox
initialViewBox =
    { x = 0
    , y = 0
    , zoom = 1
    , focusX = 0
    , focusY = 0
    }


type alias Model =
    { nodes : Dict String Node
    , edges : List Edge
    , indexAlphabet : Array String
    , commandAlphabet : List Char
    , indexCounter : Int
    , mode : Mode
    , errMsg : String
    , currentCommand : String
    , windowSize : Window.Size
    , viewBox : ViewBox
    , rng : ( Int, Int )
    }


model : Model
model =
    { nodes = Dict.empty
    , edges = []
    , mode = Normal
    , indexAlphabet = Array.fromList [ "a", "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "u", "v", "w", "x", "y", "z" ]
    , commandAlphabet = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ]
    , indexCounter = 0
    , errMsg = ""
    , currentCommand = ""
    , windowSize = { width = 0, height = 0 }
    , viewBox = initialViewBox
    , rng = ( 1, 1 ) -- Use fib sequence to generate rng values
    }


init : ( Model, Cmd Msg )
init =
    ( model, Window.size |> Task.perform WindowSize )



---- UPDATE ----


type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame dt ->
            ( { model
                | rng = Rng.next model.rng
                , nodes = updateNodes dt model.nodes model.edges
                , viewBox = moveViewBox model.viewBox dt
              }
            , Cmd.none
            )

        KeyDown key ->
            ( applyKey 1 key model, Cmd.none )

        KeyUp key ->
            ( model, Cmd.none )

        WindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )


incrementIdx : Model -> Model
incrementIdx model =
    { model | indexCounter = model.indexCounter + 1 }


insertNode : Model -> Model
insertNode model =
    let
        --- Need to fix issue where nodes fly off if they have
        --- the exact same x,y
        maxOffset =
            200

        xRand =
            (first model.rng) % maxOffset |> toFloat

        yRand =
            (second model.rng) % maxOffset |> toFloat

        i =
            buildIdx model.indexCounter model.indexAlphabet

        newX =
            (toFloat (model.windowSize.width) + xRand) / 2

        newY =
            (toFloat (model.windowSize.height) + yRand) / 2

        newNode =
            { initialNode
                | x = newX
                , y = newY
                , idx = i
            }
    in
        incrementIdx { model | nodes = Dict.insert i newNode model.nodes }


moveViewBox : ViewBox -> Float -> ViewBox
moveViewBox currentViewBox dt =
    let
        curPos =
            ( currentViewBox.x, currentViewBox.y )

        focusPos =
            ( currentViewBox.focusX, currentViewBox.focusY )

        dist =
            distance curPos focusPos

        dir =
            Physics.direction curPos focusPos

        moveIfFartherThan =
            1.0

        topSpeedRadius =
            10.0

        speed =
            Basics.max ((dist - topSpeedRadius) * 0.005) 0

        newPos =
            moveTowards curPos dir speed dt

        newX =
            first newPos

        newY =
            second newPos
    in
        if dist > moveIfFartherThan then
            { currentViewBox | x = newX, y = newY }
        else
            currentViewBox



--- Vim-esque command processeing


executeNormalCommand : Model -> Model
executeNormalCommand model =
    let
        --- Are we creating an edge?
        pieces =
            String.split "t" model.currentCommand

        --- Or labeling a node?
        startNodeLabel =
            String.startsWith "ln" model.currentCommand

        nodeToLabel =
            String.dropLeft 2 model.currentCommand

        --- Or labeling an edge?
        startEdgeLabel =
            String.startsWith "le" model.currentCommand

        edgeToLabel =
            String.dropLeft 2 model.currentCommand
    in
        if startNodeLabel then
            let
                targetNode =
                    Dict.get nodeToLabel model.nodes
            in
                case targetNode of
                    Just n ->
                        let
                            winWidth =
                                toFloat model.windowSize.width

                            winHeight =
                                toFloat model.windowSize.height

                            focusedViewBox =
                                getPointToFocusViewBox ( n.x, n.y ) winWidth winHeight
                                    |> setViewBoxFocus model.viewBox
                        in
                            { model | viewBox = focusedViewBox, currentCommand = "", mode = LabelNode }

                    Nothing ->
                        Debug.crash "Couldn't find node when labeling"
        else if startEdgeLabel then
            { model | currentCommand = "add edge label " ++ edgeToLabel }
        else if List.length pieces == 2 then
            let
                srcIdx =
                    case (List.head pieces) of
                        Just n ->
                            n

                        Nothing ->
                            ""

                destIdx =
                    case (List.drop 1 pieces |> List.head) of
                        Just n ->
                            n

                        Nothing ->
                            ""

                nodes =
                    Dict.values model.nodes

                srcNode =
                    findNodeByIdx nodes srcIdx

                destNode =
                    findNodeByIdx nodes destIdx
            in
                case srcNode of
                    Just src ->
                        case destNode of
                            Just dest ->
                                let
                                    isSameNode =
                                        src.idx == dest.idx

                                    key =
                                        model.currentCommand

                                    edgeAlreadyExists =
                                        (List.filter (\e -> e.key == key) model.edges |> List.length) > 0

                                    newEdge =
                                        { initialEdge | src = src.idx, dest = dest.idx, key = model.currentCommand }
                                in
                                    if (isSameNode) then
                                        { model | currentCommand = "", errMsg = "Cannot create edge to same node currently" }
                                    else if (edgeAlreadyExists) then
                                        { model | currentCommand = "", errMsg = "Edge already exists" }
                                    else
                                        { model | currentCommand = "", edges = List.append model.edges [ newEdge ] }

                            Nothing ->
                                { model | currentCommand = "", errMsg = "destination node doesn't exist" }

                    Nothing ->
                        { model | currentCommand = "", errMsg = "source node doesn't exist" }
        else
            { model | currentCommand = "" }


buildCommand : Keyboard.KeyCode -> Model -> Model
buildCommand keyCode model =
    let
        c =
            Char.fromCode keyCode |> toLower
    in
        if List.member c model.commandAlphabet then
            { model | currentCommand = model.currentCommand ++ (String.fromList [ c ]) }
        else
            model


applyKey : Int -> Keyboard.KeyCode -> Model -> Model
applyKey scale keyCode model =
    let
        key =
            getKey keyCode
    in
        case model.mode of
            Normal ->
                case key of
                    Ilwr ->
                        insertNode model

                    Return ->
                        executeNormalCommand model

                    _ ->
                        buildCommand keyCode model

            LabelNode ->
                case key of
                    Escape ->
                        { model | mode = Normal }

                    _ ->
                        model

            LabelEdge ->
                case key of
                    Escape ->
                        { model | mode = Normal }

                    _ ->
                        model


setViewBoxFocus : ViewBox -> ( Float, Float ) -> ViewBox
setViewBoxFocus viewBox pos =
    { viewBox | focusX = first pos, focusY = second pos }


getPointToFocusViewBox : ( Float, Float ) -> Float -> Float -> ( Float, Float )
getPointToFocusViewBox coordPoint width height =
    let
        x =
            first coordPoint

        y =
            second coordPoint

        viewX =
            x - (width * 0.5)

        viewY =
            y - (height * 0.5)
    in
        ( viewX, viewY )



---- VIEW ----


view : Model -> Html Msg
view model =
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
            List.map (\e -> edgeToSvg e model.nodes) model.edges

        elements =
            List.append nodesSvg edgesSvg

        viewX =
            model.viewBox.x |> toString

        viewY =
            model.viewBox.y |> toString

        viewBoxAttr =
            String.join " " [ viewX, viewY, viewWidth, viewHeight ]
    in
        case model.mode of
            Normal ->
                div []
                    [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                    , debugFocus [] [ Html.Styled.text "Normal" ]
                    , svg [ width viewWidth, height viewHeight, viewBox viewBoxAttr ] elements
                    ]

            LabelNode ->
                div []
                    [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                    , debugFocus [] [ Html.Styled.text "Label Node" ]
                    , svg [ width viewWidth, height viewHeight, viewBox viewBoxAttr ] elements
                    ]

            LabelEdge ->
                div []
                    [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                    , debugFocus [] [ Html.Styled.text "Label Edge" ]
                    , svg [ width viewWidth, height viewHeight, viewBox viewBoxAttr ] elements
                    ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> Html.Styled.toUnstyled
        , init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs Frame
                    , Window.resizes WindowSize
                    ]
        }
