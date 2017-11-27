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


type Key
    = LeftArrow
    | Escape
    | Elwr
    | Nlwr
    | Other


getKey : Keyboard.KeyCode -> Key
getKey keyCode =
    case keyCode of
        27 ->
            Escape

        69 ->
            Elwr

        78 ->
            Nlwr

        _ ->
            Other


type Mode
    = Normal
    | Edge
    | Node



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
                { src : Node
                , dest : Node
                }
            )
        )


type alias Model =
    { nodes : List Node
    , edges : List Edge
    , mode : Mode
    , windowSize : Window.Size
    }


model : Model
model =
    { nodes = []
    , edges = []
    , mode = Normal
    , windowSize = { width = 0, height = 0 }
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
            ( { model | nodes = List.map (\n -> applyPhysics dt model.nodes n) model.nodes }, Cmd.none )

        KeyDown key ->
            ( applyKey 1 key model, Cmd.none )

        KeyUp key ->
            ( applyKey 0 key model, Cmd.none )

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


getDistanceForces : Node -> List Node -> List ( Float, Float )
getDistanceForces node nodes =
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
                else if dist == 0 then
                    repulse 1 dir minRadius
                else
                    repulse dist dir minRadius
        )
        nodes


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


applyPhysics : Float -> List Node -> Node -> Node
applyPhysics dt nodes node =
    let
        distanceForces =
            getDistanceForces node nodes

        sumForce =
            log "Result force " (sumForces distanceForces)

        vx =
            first sumForce

        vy =
            second sumForce
    in
        --- Calculate force to apply based on distance and direction
        { node | x = node.x + vx * dt, y = node.y + vy * dt }


applyKey : Int -> Keyboard.KeyCode -> Model -> Model
applyKey scale keyCode model =
    case model.mode of
        Node ->
            model

        Edge ->
            model

        Normal ->
            let
                key =
                    getKey keyCode
            in
                case key of
                    Elwr ->
                        model

                    Nlwr ->
                        let
                            len =
                                List.length model.nodes

                            newNode =
                                { initialNode | x = toFloat (len * 10), y = toFloat (len * 10), idx = toString (List.length model.nodes) }
                        in
                            { model | nodes = List.append model.nodes [ newNode ] }

                    _ ->
                        model


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
    in
        rect [ x xPos, y yPos, width nWidth, height nHeight, rx roundX, ry roundY ] []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        winWidth =
            toString model.windowSize.width

        winHeight =
            toString model.windowSize.height

        nodesSvg =
            List.map (\n -> nodeToSvg n) model.nodes

        viewBoxAttr =
            String.join " " [ "0 0", winWidth, winHeight ]
    in
        svg [ width winWidth, height winHeight, viewBox viewBoxAttr ]
            nodesSvg



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
