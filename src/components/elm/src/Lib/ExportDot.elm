module ExportDot exposing (exportToDot, inchesToPixels)

import Types exposing (..)


exportToDot nodes edges =
    let
        nodeStr =
            nodesToDot nodes

        edgeStr =
            edgesToDot edges

        content =
            String.join "\n" [ nodeStr, edgeStr ]

        dot =
            String.concat [ "digraph SomeGraph ", "{\n", content, "\n}" ]
    in
        dot


edgesToDot edges =
    List.map
        (\edge ->
            case edge.label of
                "" ->
                    { edge | label = edge.key }

                _ ->
                    edge
        )
        edges
        |> List.map
            (\edge ->
                String.concat [ "\t", edge.src, "->", edge.dest, "[label=\"", edge.label, "\"]" ]
            )
        |> String.join "\n"


nodesToDot nodes =
    List.map
        (\node ->
            String.concat [ node.idx, nodeStyling node.label node.diminsions ]
        )
        nodes
        |> List.map (\str -> "\t" ++ str)
        |> String.join "\n"



{- Given a node, craft a graphviz styling string
   The goal being to get the website styling and the graphviz styling as
   close together as possible, ensuring the graph produced looks nice.
-}


nodeStyling label ( width, height ) =
    let
        ( widthStr, heightStr ) =
            ( toString width, toString height )

        styleStr =
            [ ( "label", "\"" ++ label ++ "\"" )
            , ( "shape", "box" )
            , ( "width", widthStr )
            , ( "height", heightStr )
            ]
                |> List.map (\( attribute, value ) -> attribute ++ "=" ++ value)
                |> String.join " "
    in
        "[" ++ styleStr ++ "]"



--"[label=\"" ++ node.label ++ "\" shape=box]"


inchesToPixels inches =
    inches * 96.0 |> floor
