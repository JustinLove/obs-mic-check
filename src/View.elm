module View exposing (view, ViewMsg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type ViewMsg
  = Connect

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div []
    [ button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    ]
