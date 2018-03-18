module State exposing (..)

import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Keyboard exposing (..)
import Physics exposing (attract, direction, distance, moveTowards, repulse, sumForces)
import Task
import Time exposing (Time)
import Tuple exposing (..)
import Window
import Rng exposing (next)
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( model, Window.size |> Task.perform WindowSize )



---- UPDATE ----


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

        EditNodeMsg nodeIdx newLabel ->
            let
                updatedNodes =
                    Dict.update nodeIdx (Maybe.map (\n -> { n | label = newLabel })) model.nodes
            in
                ( { model | nodes = updatedNodes }, Cmd.none )


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
                            { model | viewBox = focusedViewBox, currentCommand = "", mode = (LabelNode n.idx) }

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

            LabelNode idx ->
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
