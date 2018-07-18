module ExportDot exposing (exportToDot, inchesToPixels)

import Types exposing (..)


exportToDot : List Node -> List Edge -> String
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


edgesToDot : List Edge -> String
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


nodesToDot : List Node -> String
nodesToDot nodes =
    List.map
        (\node ->
            String.concat [ node.idx, nodeStyling node ]
        )
        nodes
        |> List.map (\str -> "\t" ++ str)
        |> String.join "\n"



{- Given a node, craft a graphviz styling string
   The goal being to get the website styling and the graphviz styling as
   close together as possible, ensuring the graph produced looks nice.
-}


nodeStyling : Node -> NodeStyling
nodeStyling node =
    let
        styleStr =
            [ ( "label", "\"" ++ node.label ++ "\"" )
            , ( "shape", "box" )
            , ( "width", toString node.width )
            , ( "height", toString node.height )
            ]
                |> List.map (\it -> Tuple.first it ++ "=" ++ Tuple.second it)
                |> String.join " "
    in
        "[" ++ styleStr ++ "]"



--"[label=\"" ++ node.label ++ "\" shape=box]"


inchesToPixels : Inches -> NumPixels
inchesToPixels inches =
    inches * 96.0 |> floor
