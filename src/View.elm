module View exposing (view, ViewMsg(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import AlarmRule exposing (RuleSet(..), AlarmRule(..), VideoState(..), AudioRule(..), AudioState(..), Alarm(..), matchesVideoSource)

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
      [ ("alarms", alarming model.alarm)
      ]
    ]
    [ node "style" [] [ text css ]
    , input [ type_ "password", on "change" (Json.Decode.map SetPassword (Json.Decode.at ["target", "value"] Json.Decode.string)) ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , ul [] <| violated model.time model.alarm model.activeAudioRule
    , displayScene model.ruleSet model.currentScene
    ]

violated : Int -> Alarm -> AudioRule -> List (Html ViewMsg)
violated time alarm audioRule =
  let (AudioRule _ timeout) = audioRule in
  case alarm of
    Silent -> []
    Violation start ->
      [ displayAudioRule audioRule
      , text " "
      , text <| toString (timeout - (time - start))
      ]
    Alarming start ->
      [ displayAudioRule audioRule
      , text " "
      , text <| toString (time - start)
      ]

alarming : Alarm -> Bool
alarming alarm =
  case alarm of
    Silent -> False
    Violation _ -> False
    Alarming _ -> True

displayScene : RuleSet -> Scene -> Html ViewMsg
displayScene (RuleSet default rules) scene =
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

displayVideoRule : VideoState -> Html ViewMsg
displayVideoRule videoState =
  case videoState of 
    VideoState sourceName render ->
      span []
        [ renderStatus render
        , text " "
        , text sourceName
        ]

displayAudioRule : AudioRule -> Html ViewMsg
displayAudioRule (AudioRule audioState timeout) =
  span []
    [ displayAudioState audioState
    , text " > "
    , text <| toString timeout
    , text " seconds"
    ]

displayAudioState : AudioState -> Html ViewMsg
displayAudioState audioState =
  case audioState of 
    AudioState sourceName audio ->
      span []
        [ text sourceName
        , text " "
        , audioStatus audio
        ]
    AnyAudio states ->
      span [] <| List.concat
        [ [ text "any[" ]
        , List.intersperse (text ", ") <| List.map displayAudioState states
        , [ text "]" ]
        ]
    AllAudio states ->
      span [] <| List.concat
        [ [ text "all[" ]
        , List.intersperse (text ", ") <| List.map displayAudioState states
        , [ text "]" ]
        ]
