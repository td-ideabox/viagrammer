module Main exposing (..)

import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Task
import Time exposing (Time)
import Window
import Html
import Html.Styled exposing (..)
import View exposing (..)
import Types exposing (..)
import AnimationFrame exposing (..)
import Array exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Keyboard exposing (..)
import Physics exposing (attract, direction, distance, moveTowards, repulse, sumForces)
import Task
import Tuple exposing (..)
import Window
import Rng exposing (next)
import Types exposing (..)
import Geometry exposing (..)
import Result exposing (..)
import GraphViz exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import ExportDot exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { view = View.view >> Html.Styled.toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


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
            case decodeString graphDecoder str of
                Ok layout ->
                    let
                        anchoredNodes =
                            List.map
                                (\o ->
                                    case Dict.get o.name model.nodes of
                                        Just n ->
                                            ( o.name, updateNodeWithObjectData o n )

                                        Nothing ->
                                            Debug.crash ("Couldn't find node when anchoring " ++ o.name)
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



{- Handle user input -}


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


insertNode : Model -> Model
insertNode model =
    let
        --- Need to fix issue where nodes fly off if they have
        --- the exact same x,y
        maxOffset =
            200

        ( xRng, yRng ) =
            model.rng

        ( xRand, yRand ) =
            ( xRng % maxOffset |> toFloat, yRng % maxOffset |> toFloat )

        i =
            buildIdx model.indexCounter model.indexAlphabet

        ( width, height ) =
            ( model.windowSize.width, model.windowSize.height )

        ( newX, newY ) =
            ( (toFloat (width) + xRand) / 2
            , (toFloat (height) + yRand) / 2
            )

        node =
            newNode i "#000" ( newX, newY ) Nothing False ( 1.0, 1.0 ) ""
    in
        { model | indexCounter = model.indexCounter + 1, nodes = Dict.insert i node model.nodes }


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

        ( newX, newY ) =
            newPos
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
        ( srcIdx, destIdx ) =
            srcDestIdx

        ( srcNode, destNode ) =
            ( Dict.get srcIdx nodes
            , Dict.get destIdx nodes
            )

        key =
            srcIdx ++ "t" ++ destIdx
    in
        case srcNode of
            Just src ->
                case destNode of
                    Just dest ->
                        case Dict.get key model.edges of
                            Just e ->
                                Ok e

                            Nothing ->
                                if src.idx == dest.idx then
                                    Err "Cannot create edge to same node currently"
                                else
                                    newEdge key "#0f0" "" src.idx dest.idx Pointed
                                        |> Ok

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
                        | viewBox = focusViewBox n.position model.windowSize model.viewBox
                        , currentCommand = ""
                        , mode = (EditNode n)
                    }

            Nothing ->
                Err "Couldn't find node when labeling"


executeEditEdgeCommand : String -> Model -> Result Error Model
executeEditEdgeCommand elementIdx model =
    case Dict.get elementIdx model.edges of
        Just edge ->
            case Dict.get edge.src model.nodes of
                Just sourceNode ->
                    case Dict.get edge.dest model.nodes of
                        Just destNode ->
                            let
                                midPoint =
                                    lineMidPoint sourceNode.position destNode.position
                            in
                                Ok
                                    { model
                                        | viewBox = focusViewBox midPoint model.windowSize model.viewBox
                                        , currentCommand = ""
                                        , mode = (EditEdge edge)
                                    }

                        Nothing ->
                            Err ("Couldn't find destNode " ++ edge.dest ++ " of edge " ++ edge.key)

                Nothing ->
                    Err ("Couldn't find srcNode " ++ edge.src ++ " of edge " ++ edge.key)

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


setViewBoxFocus : ViewBox -> ( Float, Float ) -> ViewBox
setViewBoxFocus viewBox ( x, y ) =
    { viewBox | focusX = x, focusY = y }


setViewBoxOrigin : ViewBox -> ViewBox
setViewBoxOrigin viewBox =
    { viewBox | originX = viewBox.x, originY = viewBox.y }


focusViewBox : ( Float, Float ) -> Window.Size -> ViewBox -> ViewBox
focusViewBox coordPoint windowSize viewBox =
    getPointToFocusViewBox coordPoint windowSize
        |> setViewBoxFocus viewBox
        |> setViewBoxOrigin


getPointToFocusViewBox : ( Float, Float ) -> Window.Size -> ( Float, Float )
getPointToFocusViewBox coordPoint windowSize =
    let
        ( x, y ) =
            coordPoint

        ( halfWindowWidth, halfWindowHeight ) =
            ( windowSize.width |> toFloat |> (*) 0.5
            , windowSize.height |> toFloat |> (*) 0.5
            )
    in
        ( x - halfWindowWidth, y - halfWindowHeight )


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



{- Calculate all the forces acting on node (changing based whether it has an
   anchor), sum them, and finally update the node
-}


applyPhysics : Float -> Dict String Node -> Dict String Edge -> Node -> Node
applyPhysics dt nodes edges node =
    let
        forcesOnNode =
            calcForcesOnNode node nodes edges

        finalForce =
            sumForces forcesOnNode

        ( x, y ) =
            node.position

        ( vx, vy ) =
            finalForce
    in
        --- Calculate force to apply based on distance and direction
        { node | position = ( x + vx * dt, y + vy * dt ) }


calcForcesOnNode : Node -> Dict String Node -> Dict String Edge -> List ( Float, Float )
calcForcesOnNode node nodes edges =
    let
        anchorAttraction =
            calcAnchorAttractions node

        nodeRepulses =
            Dict.values nodes
                |> calcNodeRepulsions node

        edgeAttractions =
            List.filter (\e -> e.src == node.idx) (Dict.values edges)
                |> calcEdgeAttractions node nodes
    in
        case node.anchorCoord of
            Just anchor ->
                List.append nodeRepulses [ anchorAttraction ]

            Nothing ->
                List.append nodeRepulses edgeAttractions


calcAnchorAttractions : Node -> ( Float, Float )
calcAnchorAttractions node =
    case node.anchorCoord of
        Just anchor ->
            let
                p1 =
                    node.position

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


calcNodeRepulsions : Node -> List Node -> List ( Float, Float )
calcNodeRepulsions node nodes =
    List.map
        (\n ->
            let
                p1 =
                    node.position

                p2 =
                    n.position

                dist =
                    distance p1 p2

                dir =
                    direction p1 p2

                minRadius =
                    calcNodeRepulseRadius node
            in
                --- Don't apply a force to yourself or if you outside the
                --- affected radius
                if n.idx == node.idx || dist > minRadius || node.ignoreForces then
                    ( 0, 0 )
                else
                    repulse dist dir minRadius
        )
        nodes


calcEdgeAttractions : Node -> Dict String Node -> List Edge -> List ( Float, Float )
calcEdgeAttractions node nodes edgeList =
    List.map
        (\e ->
            let
                src =
                    Dict.get e.src nodes

                p1 =
                    case src of
                        Just s ->
                            s.position

                        Nothing ->
                            Debug.crash (e.src ++ " is not in the node map!")

                dest =
                    Dict.get e.dest nodes

                p2 =
                    case dest of
                        Just d ->
                            d.position

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
        edgeList


calcNodeRepulseRadius : Node -> Float
calcNodeRepulseRadius node =
    let
        ( widthInches, heightInches ) =
            node.diminsions

        ( halfWidth, halfHeight ) =
            ( inchesToPixels widthInches
                |> toFloat
            , inchesToPixels heightInches
                |> toFloat
            )

        hypot =
            (halfWidth ^ 2) + (halfHeight ^ 2) |> sqrt

        buffer =
            5.0
    in
        hypot + buffer
