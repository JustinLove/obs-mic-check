module View exposing (view, ViewMsg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type ViewMsg
  = None

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div []
    [ text "stuff"
    ]
