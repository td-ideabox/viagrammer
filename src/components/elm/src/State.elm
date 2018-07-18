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
import Geometry exposing (..)
import Result exposing (..)
import Ports exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import ExportDot exposing (..)


init : ( Model, Cmd Msg )
init =
    ( model, Window.size |> Task.perform WindowSize )



---- SUBSCRIPTIONS -----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Frame
        , Window.resizes WindowSize
        , layoutData UpdateLayout
        ]


edgeDecoder : Decoder EdgeData
edgeDecoder =
    decode EdgeData
        |> required "tail" int
        |> required "head" int


objectDecoder : Decoder ObjectData
objectDecoder =
    decode ObjectData
        |> required "name" string
        |> required "pos" string


graphDecoder : Decoder GraphData
graphDecoder =
    decode GraphData
        |> optional "edges" (list edgeDecoder) []
        |> optional "objects" (list objectDecoder) []



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

        UpdateLayout str ->
            let
                layoutResult =
                    decodeString graphDecoder str
            in
                case layoutResult of
                    Ok layout ->
                        let
                            anchoredNodes =
                                List.map
                                    (\o ->
                                        let
                                            n =
                                                Dict.get o.name model.nodes
                                        in
                                            case n of
                                                Just n ->
                                                    ( o.name, updateNodeWithObjectData o n )

                                                Nothing ->
                                                    Debug.crash ("Blah " ++ o.name)
                                    )
                                    layout.objects
                                    |> Dict.fromList
                        in
                            ( { model | nodes = Dict.union anchoredNodes model.nodes }, Cmd.none )

                    Err _ ->
                        ( { model | errMsg = "Couldn't decode " ++ str }, Cmd.none )

        KeyDown key ->
            applyKey 1 key model

        KeyUp key ->
            ( model, Cmd.none )

        WindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        EditNodeMsg node newLabel ->
            let
                updatedNodes =
                    Dict.update node.idx (Maybe.map (\n -> { n | label = newLabel })) model.nodes
            in
                ( { model | nodes = updatedNodes }, Cmd.none )

        EditEdgeMsg edge newLabel ->
            let
                updatedEdges =
                    Dict.update edge.key (Maybe.map (\e -> { e | label = newLabel })) model.edges
            in
                ( { model | edges = updatedEdges }, Cmd.none )


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
            0.1

        speed =
            Basics.max ((dist) * 0.005) 0

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
        isEditCommand =
            String.startsWith "e" model.currentCommand

        isRemoveCommand =
            String.startsWith "d" model.currentCommand
    in
        if isEditCommand then
            let
                executeEditCommandRes =
                    executeEditCommand model
            in
                case executeEditCommandRes of
                    Ok newModel ->
                        newModel

                    Err msg ->
                        { model | errMsg = msg, currentCommand = "" }
        else if isRemoveCommand then
            executeRemoveCommand model
        else
            let
                --- Are we creating an edge?
                pieces =
                    String.split "t" model.currentCommand

                srcIdx =
                    case (List.head pieces) of
                        Just n ->
                            Ok n

                        Nothing ->
                            Err ("Missing src idx in " ++ model.currentCommand)

                destIdx =
                    case (List.drop 1 pieces |> List.head) of
                        Just n ->
                            Ok n

                        Nothing ->
                            Err ("Missing dest idx in " ++ model.currentCommand)
            in
                case srcIdx of
                    Ok s ->
                        case destIdx of
                            Ok d ->
                                if List.length pieces == 2 then
                                    let
                                        executeInsertEdgeCommandRes =
                                            executeInsertEdgeCommand ( s, d ) model.nodes model.edges
                                    in
                                        case executeInsertEdgeCommandRes of
                                            Ok edge ->
                                                { model | edges = Dict.insert edge.key edge model.edges, currentCommand = "" }

                                            Err msg ->
                                                { model | errMsg = msg, currentCommand = "" }
                                else
                                    { model | currentCommand = "" }

                            Err msg ->
                                { model | errMsg = msg, currentCommand = "" }

                    Err msg ->
                        { model | errMsg = msg, currentCommand = "" }


executeInsertEdgeCommand : ( String, String ) -> Dict String Node -> Dict String Edge -> Result Error Edge
executeInsertEdgeCommand srcDestIdx nodes edges =
    let
        srcIdx =
            first srcDestIdx

        destIdx =
            second srcDestIdx

        srcNode =
            findNodeByIdx nodes srcIdx

        destNode =
            findNodeByIdx nodes destIdx

        key =
            srcIdx ++ "t" ++ destIdx
    in
        case srcNode of
            Just src ->
                case destNode of
                    Just dest ->
                        let
                            isSameNode =
                                src.idx == dest.idx

                            edgeAlreadyExists =
                                Dict.get key model.edges

                            newEdge =
                                { initialEdge | src = src.idx, dest = dest.idx, key = key }
                        in
                            case edgeAlreadyExists of
                                Just e ->
                                    Ok e

                                Nothing ->
                                    if (isSameNode) then
                                        Err "Cannot create edge to same node currently"
                                    else
                                        Ok newEdge

                    Nothing ->
                        Err "destination node doesn't exist"

            Nothing ->
                Err "source node doesn't exist"


executeRemoveCommand : Model -> Model
executeRemoveCommand model =
    let
        removeElementIdx =
            String.dropLeft 1 model.currentCommand

        updatedNodes =
            Dict.remove removeElementIdx model.nodes

        updatedEdges =
            Dict.remove removeElementIdx model.edges
    in
        { model | nodes = updatedNodes, edges = updatedEdges, currentCommand = "" }


executeEditCommand : Model -> Result Error Model
executeEditCommand model =
    let
        --- Are we creating an edge?
        pieces =
            String.split "t" model.currentCommand

        isEdgeCommand =
            (List.length pieces) == 2

        editElementIdx =
            String.dropLeft 1 model.currentCommand
    in
        if isEdgeCommand then
            executeEditEdgeCommand editElementIdx model
        else
            executeEditNodeCommand editElementIdx model


executeEditNodeCommand : String -> Model -> Result Error Model
executeEditNodeCommand elementIdx model =
    let
        targetNode =
            Dict.get elementIdx model.nodes
    in
        case targetNode of
            Just n ->
                Ok
                    { model
                        | viewBox = focusViewBox ( n.x, n.y ) model.windowSize model.viewBox
                        , currentCommand = ""
                        , mode = (EditNode n)
                    }

            Nothing ->
                Err "Couldn't find node when labeling"


executeEditEdgeCommand : String -> Model -> Result Error Model
executeEditEdgeCommand elementIdx model =
    let
        targetEdge =
            Dict.get elementIdx model.edges
    in
        case targetEdge of
            Just e ->
                let
                    srcNode =
                        Dict.get e.src model.nodes
                in
                    case srcNode of
                        Just n1 ->
                            let
                                destNode =
                                    Dict.get e.dest model.nodes
                            in
                                case destNode of
                                    Just n2 ->
                                        let
                                            midPoint =
                                                lineMidPoint ( n1.x, n1.y ) ( n2.x, n2.y )
                                        in
                                            Ok
                                                { model
                                                    | viewBox = focusViewBox ( first midPoint, second midPoint ) model.windowSize model.viewBox
                                                    , currentCommand = ""
                                                    , mode = (EditEdge e)
                                                }

                                    Nothing ->
                                        Err ("Couldn't find destNode " ++ e.dest ++ " of edge " ++ e.key)

                        Nothing ->
                            Err ("Couldn't find srcNode " ++ e.src ++ " of edge " ++ e.key)

            Nothing ->
                Err ("Edge " ++ elementIdx ++ " doesn't appear to exist")


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


applyKey : Int -> Keyboard.KeyCode -> Model -> ( Model, Cmd Msg )
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
                            nodes =
                                Dict.values model.nodes

                            edges =
                                Dict.values model.edges

                            dot =
                                exportToDot nodes edges
                        in
                            ( insertNode model, sendDot dot )

                    Return ->
                        ( executeNormalCommand model, Cmd.none )

                    _ ->
                        ( buildCommand keyCode model, Cmd.none )

            EditNode node ->
                case key of
                    Escape ->
                        ( { model | mode = Normal }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            EditEdge edge ->
                case key of
                    Escape ->
                        ( { model | mode = Normal }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )


setViewBoxFocus : ViewBox -> ( Float, Float ) -> ViewBox
setViewBoxFocus viewBox pos =
    { viewBox | focusX = first pos, focusY = second pos }


setViewBoxOrigin : ViewBox -> ViewBox
setViewBoxOrigin viewBox =
    { viewBox | originX = viewBox.x, originY = viewBox.y }


focusViewBox : ( Float, Float ) -> Window.Size -> ViewBox -> ViewBox
focusViewBox coordPoint windowSize viewBox =
    getPointToFocusViewBox ( first coordPoint, second coordPoint ) windowSize
        |> setViewBoxFocus viewBox
        |> setViewBoxOrigin


getPointToFocusViewBox : ( Float, Float ) -> Window.Size -> ( Float, Float )
getPointToFocusViewBox coordPoint windowSize =
    let
        x =
            first coordPoint

        y =
            second coordPoint

        width =
            windowSize.width |> toFloat

        height =
            windowSize.height |> toFloat

        viewX =
            x - (width * 0.5)

        viewY =
            y - (height * 0.5)
    in
        ( viewX, viewY )


findNodeByIdx : Dict String Node -> String -> Maybe Node
findNodeByIdx nodes idx =
    Dict.get idx nodes


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


updateNodes : Float -> Dict String Node -> Dict String Edge -> Dict String Node
updateNodes dt nodes edges =
    Dict.map
        (\k v ->
            applyPhysics dt nodes edges v
        )
        nodes



-- This is probably suuuuuper slow right now, n^2 at least


calcForcesOnNode : Node -> Dict String Node -> Dict String Edge -> List ( Float, Float )
calcForcesOnNode node nodes edges =
    let
        anchorAttraction =
            case node.anchorCoord of
                Just anchor ->
                    let
                        p1 =
                            ( node.x, node.y )

                        p2 =
                            anchor

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

                Nothing ->
                    ( 0, 0 )

        nodeRepulses =
            Dict.values nodes
                |> List.map
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

        edgeAttractions =
            List.filter (\e -> e.src == node.idx) (Dict.values edges)
                |> List.map
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
    in
        case node.anchorCoord of
            Just anchor ->
                List.append nodeRepulses [ anchorAttraction ]

            Nothing ->
                List.append nodeRepulses edgeAttractions


applyPhysics : Float -> Dict String Node -> Dict String Edge -> Node -> Node
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
