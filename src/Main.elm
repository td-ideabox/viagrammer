module Main exposing (..)

import Debug exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Color exposing (..)
import Time exposing (Time)
import AnimationFrame exposing (..)
import Window
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Window
import Keyboard exposing (..)
import Tuple exposing (..)
import Char exposing (..)
import Array exposing (..)
import Dict exposing (..)


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


type alias Indexed a =
    { a | idx : String }


type alias Positioned a =
    { a | x : Float, y : Float }


type alias Moved a =
    { a | vx : Float, vy : Float }


type alias Labeled a =
    { a | label : String }


type alias Colored a =
    { a | color : String }


type alias Node =
    Indexed
        (Positioned
            (Labeled
                (Colored
                    { width : Int
                    , height : Int
                    , roundX : Int
                    , roundY : Int
                    }
                )
            )
        )


initialNode : Node
initialNode =
    { idx = "unassigned"
    , x = 0
    , y = 0
    , label = "unlabeled"
    , color = "#f00"
    , width = 80
    , height = 80
    , roundX = 15
    , roundY = 15
    }


type alias Edge =
    Indexed
        (Colored
            (Labeled
                { src : String
                , dest : String
                }
            )
        )


initialEdge : Edge
initialEdge =
    { idx = "unassigned edge"
    , color = "#0f0"
    , label = "unlabeled edge"
    , src = ""
    , dest = ""
    }


type alias Model =
    { nodes : Dict String Node
    , edges : List Edge
    , mode : Mode
    , errMsg : String
    , currentCommand : String
    , windowSize : Window.Size
    , rng : ( Int, Int )
    }


model : Model
model =
    { nodes = Dict.empty
    , edges = []
    , mode = Normal
    , errMsg = ""
    , currentCommand = ""
    , windowSize = { width = 0, height = 0 }
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
            let
                a =
                    first model.rng

                b =
                    second model.rng

                a2 =
                    b

                b2 =
                    (a + b) % 1000

                nextRng =
                    ( a2, b2 )
            in
                ( { model
                    | rng = nextRng
                    , nodes =
                        Dict.map
                            (\k v ->
                                applyPhysics dt model.nodes model.edges v
                            )
                            model.nodes
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


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance p1 p2 =
    let
        x1 =
            first p1

        y1 =
            second p1

        x2 =
            first p2

        y2 =
            second p2
    in
        sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


direction : ( Float, Float ) -> ( Float, Float ) -> Float
direction p1 p2 =
    let
        x1 =
            first p1

        y1 =
            second p1

        x2 =
            first p2

        y2 =
            second p2
    in
        atan2 (y2 - y1) (x2 - x1)


repulse : Float -> Float -> Float -> ( Float, Float )
repulse dist dirTowards radius =
    let
        --- The closer we get the stronger the force.
        f =
            Basics.max ((radius - dist) * 0.005) 0

        dirAway =
            dirTowards - pi

        x =
            (cos dirAway) * f

        y =
            (sin dirAway) * f
    in
        ( x, y )


attract : Float -> Float -> Float -> ( Float, Float )
attract dist dirTowards radius =
    let
        f =
            Basics.max ((dist - radius) * 0.005) 0

        x =
            (cos dirTowards) * f

        y =
            (sin dirTowards) * f
    in
        ( x, y )



-- This is probably suuuuuper slow right now, n^2 at least


getDistanceForces : Node -> Dict String Node -> List Edge -> List ( Float, Float )
getDistanceForces node nodes edges =
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
                    if n.idx == node.idx || dist > minRadius then
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
                            if dist < minRadius then
                                ( 0, 0 )
                            else
                                attract dist dir minRadius
                    )
                    nodeEdges
                )


sumForces : List ( Float, Float ) -> ( Float, Float )
sumForces forces =
    List.foldr
        (\f1 f2 ->
            let
                x1 =
                    first f1

                y1 =
                    second f1

                x2 =
                    first f2

                y2 =
                    second f2
            in
                ( x1 + x2, y1 + y2 )
        )
        ( 0, 0 )
        forces


applyPhysics : Float -> Dict String Node -> List Edge -> Node -> Node
applyPhysics dt nodes edges node =
    let
        distanceForces =
            getDistanceForces node nodes edges

        sumForce =
            log "Result force " (sumForces distanceForces)

        vx =
            first sumForce

        vy =
            second sumForce
    in
        --- Calculate force to apply based on distance and direction
        { node | x = node.x + vx * dt, y = node.y + vy * dt }


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
            { model | currentCommand = "add node label " ++ nodeToLabel }
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
                    List.filter (\n -> n.idx == srcIdx) nodes |> List.head

                destNode =
                    List.filter (\n -> n.idx == destIdx) nodes |> List.head
            in
                case srcNode of
                    Just src ->
                        case destNode of
                            Just dest ->
                                let
                                    isSameNode =
                                        src.idx == dest.idx

                                    newIdx =
                                        model.currentCommand

                                    edgesWithSameIdx =
                                        List.filter (\e -> e.idx == newIdx) model.edges

                                    edgeAlreadyExists =
                                        (List.length edgesWithSameIdx) > 0

                                    newEdge =
                                        { initialEdge | src = src.idx, dest = dest.idx, idx = model.currentCommand }
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
        --- alphanumeric
        validChars =
            [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ]

        c =
            Char.fromCode keyCode |> toLower
    in
        if List.member c validChars then
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
                        let
                            newX =
                                (toFloat model.windowSize.width) / 2

                            newY =
                                (toFloat model.windowSize.height) / 2

                            --- Need to fix issue where nodes fly off if they have
                            --- the exact same x,y
                            maxOffset =
                                200

                            xRand =
                                (first model.rng) % maxOffset |> toFloat

                            yRand =
                                (second model.rng) % maxOffset |> toFloat

                            len =
                                Dict.size model.nodes

                            --- Exclude reserved chars such as 'e' and 't'
                            alphabet =
                                Array.fromList [ "a", "b", "c", "d", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "u", "v", "w", "x", "y", "z" ]

                            i =
                                buildIdx len alphabet

                            newNode =
                                { initialNode
                                    | x = newX + xRand
                                    , y = newY + yRand
                                    , idx = i
                                }
                        in
                            { model | nodes = Dict.insert i newNode model.nodes }

                    Return ->
                        executeNormalCommand model

                    _ ->
                        buildCommand keyCode model


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

        labelX =
            toString (node.x + 10)

        labelY =
            toString (node.y + 15)
    in
        g []
            [ rect [ x xPos, y yPos, width nWidth, height nHeight, rx roundX, ry roundY, fill "transparent", stroke "black" ] []
            , text_ [ x labelX, y labelY ] [ Svg.text idx ]
            ]


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
        g [] [ line [ x1 srcX, y1 srcY, x2 destX, y2 destY, stroke "black" ] [] ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        winWidth =
            toString model.windowSize.width

        winHeight =
            toString model.windowSize.height

        nodesSvg =
            Dict.map (\k v -> nodeToSvg v) model.nodes |> Dict.values

        edgesSvg =
            List.map (\e -> edgeToSvg e model.nodes) model.edges

        elements =
            List.append nodesSvg edgesSvg

        viewBoxAttr =
            String.join " " [ "0 0", winWidth, winHeight ]
    in
        svg [ width winWidth, height winHeight, viewBox viewBoxAttr ]
            elements



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
