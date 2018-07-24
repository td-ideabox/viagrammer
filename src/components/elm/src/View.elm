module View exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Tuple exposing (first, second)
import Css exposing (..)
import Types exposing (..)
import Dict exposing (..)
import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)


--We want to keep these modules separate cause otherwise it's super
--annoying to have to explicitly define whether something is an
--Svg or Html element (the Svg and Html packages overlap almost exactly)

import ViewSvg exposing (..)
import ViewHtml exposing (..)


-- Root View


view model =
    case model.mode of
        Normal ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , downloadExportButton model
                , svgCanvas model
                ]

        EditNode node ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , editNodeView model.viewBox node
                , svgCanvas model
                ]

        EditEdge edge ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , editEdgeView model.viewBox edge
                , svgCanvas model
                ]
