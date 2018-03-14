module Main exposing (..)

import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Keyboard exposing (..)
import Physics exposing (attract, direction, distance, moveTowards, repulse, sumForces)
import String exposing (toInt)
import Task
import Time exposing (Time)
import Tuple exposing (..)
import Window
import Window
import Css exposing (absolute, px, em, position, left, bold)
import Html.Styled as Html exposing (..)
import Svg.Styled as Svg exposing (g, line, rect, svg, text_)
import Svg.Styled.Attributes as SvgAttr exposing (width, height, viewBox, x, y, x1, y1, x2, y2, stroke, rx, ry, fill)
import Rng exposing (next)


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
            direction curPos focusPos

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


updateNodes : Float -> Dict String Node -> List Edge -> Dict String Node
updateNodes dt nodes edges =
    Dict.map
        (\k v ->
            applyPhysics dt nodes edges v
        )
        nodes



-- This is probably suuuuuper slow right now, n^2 at least


calcForcesOnNode : Node -> Dict String Node -> List Edge -> List ( Float, Float )
calcForcesOnNode node nodes edges =
    let
        nodeList =
            Dict.values nodes

        nodeEdges =
            List.filter (\e -> e.src == node.idx) edges
    in
        List.map
            (\n ->
                let
                    p1 =
                        ( node.x, node.y )

                    p2 =
                        ( n.x, n.y )

                    dist =
                        distance p1 p2

                    dir =
                        direction p1 p2

                    minRadius =
                        100
                in
                    --- Don't apply a force to yourself or if you outside the
                    --- affected radius
                    if n.idx == node.idx || dist > minRadius || node.ignoreForces then
                        ( 0, 0 )
                    else
                        repulse dist dir minRadius
            )
            nodeList
            |> List.append
                (List.map
                    (\e ->
                        let
                            src =
                                Dict.get e.src nodes

                            p1 =
                                case src of
                                    Just s ->
                                        ( s.x, s.y )

                                    Nothing ->
                                        Debug.crash (e.src ++ " is not in the node map!")

                            dest =
                                Dict.get e.dest nodes

                            p2 =
                                case dest of
                                    Just d ->
                                        ( d.x, d.y )

                                    Nothing ->
                                        Debug.crash (e.dest ++ " is not in the node map")

                            dist =
                                distance p1 p2

                            dir =
                                direction p1 p2

                            minRadius =
                                100
                        in
                            if dist < minRadius || node.ignoreForces then
                                ( 0, 0 )
                            else
                                attract dist dir minRadius
                    )
                    nodeEdges
                )


applyPhysics : Float -> Dict String Node -> List Edge -> Node -> Node
applyPhysics dt nodes edges node =
    let
        forcesOnNode =
            calcForcesOnNode node nodes edges

        finalForce =
            sumForces forcesOnNode

        vx =
            first finalForce

        vy =
            second finalForce
    in
        --- Calculate force to apply based on distance and direction
        { node | x = node.x + vx * dt, y = node.y + vy * dt }



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
                            { model | viewBox = focusedViewBox, currentCommand = "" }

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


nodeToSvg : Node -> Svg.Svg msg
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
            [ Svg.rect
                [ SvgAttr.x xPos
                , SvgAttr.y yPos
                , SvgAttr.width nWidth
                , SvgAttr.height nHeight
                , SvgAttr.rx roundX
                , SvgAttr.ry roundY
                , SvgAttr.fill "transparent"
                , SvgAttr.stroke "black"
                ]
                []
            , Svg.text_ [ SvgAttr.x idxX, SvgAttr.y idxY ] [ Svg.text node.idx ]
            , Svg.text_ [ SvgAttr.x debugCoordX, SvgAttr.y debugCoordY ] [ Svg.text debugCoord ]
            , Svg.text_ [ SvgAttr.x labelX, SvgAttr.y labelY ] [ Svg.text node.label ]
            ]


edgeToSvg : Edge -> Dict String Node -> Svg.Svg msg
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
        Svg.g []
            [ Svg.line
                [ SvgAttr.x1 srcX
                , SvgAttr.y1 srcY
                , SvgAttr.x2 destX
                , SvgAttr.y2 destY
                , SvgAttr.stroke "black"
                ]
                []
            ]



---- VIEW ----


debugCommand : List (Html.Attribute msg) -> List (Html msg) -> Html msg
debugCommand =
    Html.styled div [ position absolute, Css.left (Css.pct 50), Css.fontWeight Css.bold ]


debugFocus : List (Html.Attribute msg) -> List (Html msg) -> Html msg
debugFocus =
    Html.styled div
        [ position absolute
        , Css.left (Css.pct 50)
        , Css.top (Css.pct 50)
        , Css.color (Css.rgb 255 0 0)
        , Css.fontWeight Css.bold
        ]


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
        div []
            [ debugCommand [] [ Html.text model.currentCommand ]
            , debugFocus [] [ Html.text "X" ]
            , Svg.svg [ SvgAttr.width viewWidth, SvgAttr.height viewHeight, SvgAttr.viewBox viewBoxAttr ]
                elements
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
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
