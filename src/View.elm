module View exposing (view, ViewMsg(..))

import OBSWebSocket.Response as Response exposing (Scene, Source, Render(..), Audio(..))

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
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      ]
    ]
    [ renderStatus source.render
    , text " "
    , audioStatus source.audio
    , text " "
    , text source.name
    , text " "
    , em [] [ text source.type_ ]
    ]

renderStatus : Render -> Html ViewMsg
renderStatus render =
  case render of
    Visible -> span [ class "video" ] [ text "O" ]
    Hidden -> span [ class "video" ] [ text "-" ]

audioStatus : Audio -> Html ViewMsg
audioStatus audio =
  case audio of
    Muted -> span [ class "audio muted" ] [ text "<X " ]
    Live -> span [ class "audio live" ] [ text "<))" ]

