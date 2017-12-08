module View exposing (view, ViewMsg(..), AppMode(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Alarm(..), checkVideoState, checkAudioRule)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Json.Decode

type ViewMsg
  = None
  | Connect
  | SetPassword String
  | SelectConfig
  | SelectRuleVideoName VideoState
  | SelectVideoRender VideoState
  | SelectVideoSource String

type AppMode
  = Status
  | Config
  | SelectVideo VideoState

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

.rules { border: solid #aaa 1px; border-collapse: collapse;}
.rules td, .rules th {
  border-top: solid #aaa 1px;
  border-bottom: solid #aaa 1px;
  border-left: dashed #eee 1px;
  border-right: dashed #eee 1px;
  padding-left: 1em;
  padding-right: 1em;
}
.rules ul { margin: 0; }

.source-list { border: solid #aaa 1px; border-collapse: collapse;}
.source-list td, .source-list th {
  border-top: solid #aaa 1px;
  border-bottom: solid #aaa 1px;
  border-left: dashed #eee 1px;
  border-right: dashed #eee 1px;
  padding-left: 1em;
  padding-right: 1em;
}
.source-list .icon {
  border-left: none;
  border-right: none;
  padding: 0.2em;
}
.source-list .icon + td {
  border-left: none;
  padding-left: 0.2em;
}
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
    , case model.appMode of
        Status -> displayStatus model
        Config -> displayConfig model
        SelectVideo index -> displaySelectVideo model
    ]

displayHeader model =
  header []
    [ input
      [ type_ "password"
      , on "change" <| targetValue SetPassword
      ] []
    , button [ onClick Connect ] [ text "Connect" ]
    , text <| toString model.connected
    , modeControl (model.appMode /= Status)
    , if alarming model.alarm then
        audio
          [ autoplay True
          , src "167337__willy-ineedthatapp-com__pup-alert.mp3"
          ] []
      else
        text ""
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

displaySelectVideo model =
  let scene = model.currentScene in
  div [ id "select-video" ]
    [ h2 [] [ text scene.name ]
    , table [ class "source-list" ]
      <| List.map displaySourceForSelect scene.sources
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
        [ displayDefaultRule (checkAudioRule sources default) default ]
      |> List.append
        [ th [] [ text "Video Source" ]
        , th [] [ text "Audio Status" ]
        , th [] [ text "Seconds" ]
        ]
      |> table [ class "rules" ]
    ]

checkRule : List Source -> (VideoState, AudioRule) -> Bool
checkRule sources (video, audio) =
  (checkVideoState sources video) && (checkAudioRule sources audio)

alarming : Alarm -> Bool
alarming alarm =
  case alarm of
    Silent -> False
    Violation _ -> False
    Alarming _ -> True

displayScene : RuleSet -> Scene -> Html ViewMsg
displayScene ruleSet scene =
  div []
    [ h2 [] [ text scene.name ]
    , ul [] <| List.map (displaySource ruleSet) scene.sources
    ]

displaySource : RuleSet -> Source -> Html ViewMsg
displaySource ruleSet source =
  li
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
    , ruleSet
      |> RuleSet.toList
      |> List.filter (\((VideoState name _), _) -> name == source.name)
      |> List.map (displayRule False)
      |> table [ class "rules" ]
    ]

displaySourceForSelect : Source -> Html ViewMsg
displaySourceForSelect source =
  tr
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    , onClick (SelectVideoSource source.name)
    ]
    [ td [ class "icon" ] [ renderStatus source.render ]
    , td [ class "icon" ] [ audioStatus source.audio ]
    , td [] [ text source.name ]
    , td [] [ em [] [ text source.type_ ] ]
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

displayRule : Bool -> (VideoState, AudioRule) -> Html ViewMsg
displayRule active (video, audio) =
  tr [ classList [ ("active", active) ] ]
    <| List.append
      [ (displayVideoRule video) ]
      (displayAudioRule audio)

displayDefaultRule : Bool -> AudioRule -> Html ViewMsg
displayDefaultRule active audioRule =
  tr [ classList [ ("active", active) ] ]
    <| List.append
      [ td [] [ text "default " ] ]
      (displayAudioRule audioRule)

displayVideoRule : VideoState -> Html ViewMsg
displayVideoRule videoState =
  case videoState of 
    VideoState sourceName render ->
      td []
        [ a [ href "#", onClick (SelectVideoRender videoState) ]
          [ renderStatus render ]
        , text " "
        , a
          [ href "#", onClick (SelectRuleVideoName videoState) ]
          [ text sourceName ]
        ]

displayAudioRule : AudioRule -> List (Html ViewMsg)
displayAudioRule (AudioRule audioState timeout) =
  [ td [] [ displayAudioState audioState ]
  , td []
    [ text <| toString timeout
    ]
  ]

displayAudioState : AudioState -> Html ViewMsg
displayAudioState audioState =
  case audioState of 
    AudioState sourceName audio ->
      div []
        [ text sourceName
        , text " "
        , audioStatus audio
        ]
    AnyAudio states ->
      div []
        [ text "any"
        , ul [] <| List.map (\e -> li [] [e]) <| List.map displayAudioState states
        ]
    AllAudio states ->
      div []
        [ text "all"
        , ul [] <| List.map (\e -> li [] [e]) <| List.map displayAudioState states
        ]

modeControl : Bool -> Html ViewMsg
modeControl isChecked =
  span []
    [ input
      [ type_ "checkbox"
      , Html.Attributes.name "Config"
      , id "app-mode"
      , value "config"
      , onCheck (\_ -> SelectConfig)
      , checked isChecked
      ] []
    , label [ for "app-mode" ] [text "Config" ]
    ]
