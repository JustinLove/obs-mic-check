module View exposing (view, ViewMsg(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import AlarmRule exposing (AlarmRule(..), VideoRule(..), AudioRule(..), matchesVideoSource)

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
      [ ("alarms", model.alarm /= Nothing)
      ]
    ]
    [ node "style" [] [ text css ]
    , input [ type_ "password", on "change" (Json.Decode.map SetPassword (Json.Decode.at ["target", "value"] Json.Decode.string)) ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , model.alarm
      |> Maybe.map displayRule
      |> Maybe.map List.singleton
      |> Maybe.withDefault []
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
        |> List.filter (matchesVideoSource source)
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
displayRule (AlarmRule video audio) =
  li []
    [ (displayVideoRule video)
    , text " "
    , (displayAudioRule audio)
    ]

displayVideoRule : VideoRule -> Html ViewMsg
displayVideoRule videoRule =
  case videoRule of 
    SourceRule sourceName render ->
      span []
        [ renderStatus render
        , text " "
        , text sourceName
        ]
    DefaultRule ->
      span [] [ text "Default" ]


displayAudioRule : AudioRule -> Html ViewMsg
displayAudioRule audioRule =
  case audioRule of 
    AudioRule sourceName audio ->
      span []
        [ text sourceName
        , text " "
        , audioStatus audio
        ]
