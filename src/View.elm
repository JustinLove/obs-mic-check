module View exposing (view, ViewMsg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type ViewMsg
  = Test

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div []
    [ button [ onClick Test ] [ text "Test" ]
    ]
