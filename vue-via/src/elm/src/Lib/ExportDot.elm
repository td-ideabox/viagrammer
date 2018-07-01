module ExportDot exposing (exportToDot)

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
            let
                label =
                    node.label
            in
                case label of
                    "" ->
                        String.concat [ node.idx ]

                    _ ->
                        String.concat [ node.idx, "[label=\"", node.label, "\"]" ]
        )
        nodes
        |> List.map (\str -> "\t" ++ str)
        |> String.join "\n"

