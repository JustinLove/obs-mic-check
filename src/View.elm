module View exposing (view, ViewMsg(..), AppMode(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import AlarmRule exposing (RuleSet(..), AlarmRule(..), VideoState(..), AudioRule(..), AudioState(..), Alarm(..), matchesVideoSource, checkRule, checkAudioRule)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Json.Decode

type ViewMsg
  = Connect
  | SetPassword String
  | SetMode AppMode

type AppMode
  = Status
  | Config

-- VIEW

css = """
.audio.muted { color: white; background-color: red; }
.hidden { opacity: 0.5; }
.alarms { background-color: #FBB; }
.active { background-color: #FFB; }
#status { display: none; }
#config { display: none; }
.mode-status #status { display: block; }
.mode-config #config { display: block; }
"""

-- view : Model -> Html ViewMsg
view model =
  div
    [ classList
      [ ("alarms", alarming model.alarm)
      , ("mode-status", model.appMode == Status)
      , ("mode-config", model.appMode == Config)
      ]
    ]
    [ node "style" [] [ text css ]
    , displayHeader model
    , displayStatus model
    , displayConfig model
    ]

displayHeader model =
  header []
    [ input
      [ type_ "password"
      , on "change" <| targetValue SetPassword
      ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , case model.appMode of
        Status -> modeControl model.appMode Config
        Config -> modeControl model.appMode Status
    ]

displayStatus model =
  div [ id "status" ]
    [ violated model.time model.alarm model.activeAudioRule
    , displayRuleSet model.currentScene.sources model.ruleSet
    ]

displayConfig model =
  div [ id "config" ]
    [ displayScene model.ruleSet model.currentScene
    ]

targetValue : (String -> ViewMsg) -> Json.Decode.Decoder ViewMsg
targetValue tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] Json.Decode.string)

violated : Int -> Alarm -> AudioRule -> Html ViewMsg
violated time alarm audioRule =
  let (AudioRule _ timeout) = audioRule in
  case alarm of
    Silent ->
      alarmTime timeout 0
    Violation start ->
      alarmTime timeout (time - start)
    Alarming start ->
      alarmTime timeout (time - start)

alarmTime : Int -> Int -> Html ViewMsg
alarmTime max val =
  span []
    [ progress
      [ Html.Attributes.max <| toString max
      , value <| toString val
      ] []
    , text " "
    , text <| toString val
    ]

displayRuleSet : List Source -> RuleSet -> Html ViewMsg
displayRuleSet sources (RuleSet default rules) =
  div []
    [ rules
      |> List.map (\rule -> (displayRule (checkRule sources rule) rule))
      |> (flip List.append)
        [ li [ classList [ ("active", checkAudioRule sources default) ] ]
          [ text "default ", displayAudioRule default ]
        ]
      |> ol [ class "rule" ]
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
        |> List.map (displayRule False)
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

displayRule : Bool -> AlarmRule -> Html ViewMsg
displayRule active (AlarmRule video audio) =
  li [ classList [ ("active", active) ] ]
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


modeControl : AppMode -> AppMode -> Html ViewMsg
modeControl current mode =
  span []
    [ input
      [ type_ "checkbox"
      , Html.Attributes.name "Config"
      , id "app-mode"
      , value "config"
      , onCheck (\_ -> SetMode mode)
      , checked (current == Config)
      ] []
    , label [ for "app-mode" ] [text "Config" ]
    ]
