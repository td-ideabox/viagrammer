module Main exposing (..)

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
            (Moved
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
        )


initialNode : Node
initialNode =
    { idx = "unassigned"
    , x = 0
    , y = 0
    , vx = 0
    , vy = 0
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
            ( { model | nodes = List.map (\n -> applyPhysics dt n) model.nodes }, Cmd.none )

        KeyDown key ->
            ( applyKey 1 key model, Cmd.none )

        KeyUp key ->
            ( applyKey 0 key model, Cmd.none )

        WindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )


applyPhysics : Float -> Node -> Node
applyPhysics dt node =
    { node | x = node.x + 0.05 * dt, y = node.y + 0.05 * dt }


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
                        { model | nodes = List.append model.nodes [ initialNode ] }

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
