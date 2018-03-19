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


view : Model -> Html Msg
view model =
    case model.mode of
        Normal ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , debugFocus [] [ Html.Styled.text "Normal" ]
                , svgCanvas model
                ]

        LabelNode node ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , debugFocus [] [ Html.Styled.text "Label Node" ]
                , editNodeView model.viewBox node
                , svgCanvas model
                ]

        LabelEdge ->
            div []
                [ debugCommand [] [ Html.Styled.text model.currentCommand ]
                , debugFocus [] [ Html.Styled.text "Label Edge" ]
                , svgCanvas model
                ]
