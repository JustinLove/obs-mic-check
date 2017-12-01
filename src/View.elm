module View exposing (view, ViewMsg(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import AlarmRule exposing (AlarmRule)

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
.alarms { background-color: #FBB; }
"""

-- view : Model -> Html ViewMsg
view model =
  div
    [ classList
      [ ("alarms", not <| List.isEmpty model.alarms)
      ]
    ]
    [ node "style" [] [ text css ]
    , input [ type_ "password", on "change" (Json.Decode.map SetPassword (Json.Decode.at ["target", "value"] Json.Decode.string)) ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , model.alarms
      |> List.map displayRule
      |> ul [ class "rule" ]
    , displayScene model.rules model.currentScene
    ]

displayScene : List AlarmRule -> Scene -> Html ViewMsg
displayScene rules scene =
  div []
    [ h2 [] [ text scene.name ]
    , ul [] <| List.map (displaySource rules) scene.sources
    ]

displaySource : List AlarmRule -> Source -> Html ViewMsg
displaySource rules source =
  ul
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    ]
    [ h3 []
      [ renderStatus source.render
      , text " "
      , audioStatus source.audio
      , text " "
      , text source.name
      , text " "
      , em [] [ text source.type_ ]
      ]
    , rules
        |> List.filter (\rule -> rule.videoSourceName == source.name)
        |> List.map displayRule
        |> ul [ class "rule" ]
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

displayRule : AlarmRule -> Html ViewMsg
displayRule rule =
  li []
    [ renderStatus rule.render
    , text " "
    , text rule.videoSourceName
    , text " "
    , text rule.audioSourceName
    , text " "
    , audioStatus rule.audio
    ]
