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

css = """
.audio.muted { color: white; background-color: red; }
"""

-- view : Model -> Html ViewMsg
view model =
  div []
    [ node "style" [] [ text css ]
    , input [ type_ "password", on "change" (Json.Decode.map SetPassword (Json.Decode.at ["target", "value"] Json.Decode.string)) ] []
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
    , muteStatus source.muted
    , text " "
    , text source.name
    , text " "
    , em [] [ text source.type_ ]
    ]

muteStatus : Bool -> Html ViewMsg
muteStatus muted =
  if muted then
    span [ class "audio muted" ] [ text "<X " ]
  else
    span [ class "audio" ] [ text "<))" ]

