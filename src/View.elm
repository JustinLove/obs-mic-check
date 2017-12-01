module View exposing (view, ViewMsg(..))

import OBSWebSocket.Response as Response exposing (Scene, Source)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Json.Decode

type ViewMsg
  = Connect
  | SetPassword String

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div []
    [ input [ type_ "password", on "change" (Json.Decode.map SetPassword (Json.Decode.at ["target", "value"] Json.Decode.string)) ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , displayScene model.currentScene
    ]

displayScene : Scene -> Html ViewMsg
displayScene scene =
  div []
    [ h2 [] [ text scene.name ]
    , ul [] <| List.map displaySource scene.sources
    ]

displaySource : Source -> Html ViewMsg
displaySource source =
  ul []
    [ text (if source.render then "O" else "-")
    , text " "
    , text source.name
    , text " "
    , em [] [ text source.type_ ]
    ]
