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
.hidden { opacity: 0.5; }
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
  ul
    [ classList 
      [ ("hidden", not source.render)
      , ("visible", source.render)
      ]
    ]
    [ renderStatus source.render
    , text " "
    , muteStatus source.muted
    , text " "
    , text source.name
    , text " "
    , em [] [ text source.type_ ]
    ]

renderStatus : Bool -> Html ViewMsg
renderStatus render =
  if render then
    span [ class "video" ] [ text "O" ]
  else
    span [ class "video" ] [ text "-" ]

muteStatus : Bool -> Html ViewMsg
muteStatus muted =
  if muted then
    span [ class "audio muted" ] [ text "<X " ]
  else
    span [ class "audio" ] [ text "<))" ]

