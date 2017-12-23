module View exposing (view, ViewMsg(..), AppMode(..), RuleKey(..), ConnectionStatus(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), Challenge, mightBeVideoSource, mightBeAudioSource)
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..), checkVideoState, checkAudioRule)
import Alarm exposing (Alarm(..), AlarmRepeat(..), isAlarming)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Lazy exposing (lazy3)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode
import Dict
import Regex exposing (regex)

type ViewMsg
  = None
  | SetPassword String
  | LogOut
  | Cancel
  | Navigate AppMode
  | SelectVideoSource String
  | SelectRuleAudioRule RuleKey
  | SelectAudioSource String
  | SelectAudioStatus String
  | SelectAudioMode Operator
  | SetTimeout RuleKey Int
  | CopyRule RuleKey
  | RemoveRule VideoState

type RuleKey
  = VideoKey VideoState
  | DefaultKey

type AppMode
  = AudioRules
  | FrameRules
  | SelectVideo AudioRule
  | SelectAudio RuleKey Operator (List AudioState)

type ConnectionStatus
 = Disconnected
 | Connecting
 | Connected String
 | AuthRequired String Challenge
 | LoggingIn String
 | Authenticated String

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div
    [ classList
      [ ("alarms", isAlarming model.alarm)
      , ("mode-audio-rules", model.appMode == AudioRules)
      ]
    , id "application"
    ]
    [ displayHeader model
    , displayNavigation model
    , case model.appMode of
        AudioRules -> displayAudioRules model
        FrameRules -> displayFrameRules model
        SelectVideo _ -> displaySelectVideo model
        SelectAudio _ operator audioStates -> displaySelectAudio model operator audioStates
    ]

displayHeader model =
  header []
    [ displayConnectionStatus model.connected
    , alarmStatus model
    , if audioPlaying model.alarmRepeat then
        audio
          [ autoplay True
          , src "167337__willy-ineedthatapp-com__pup-alert.mp3"
          ] []
      else
        text ""
    ]

displayNavigation model =
  nav []
    [ ul []
      [ navigationItem model.appMode AudioRules "Audio Alarms"
      , navigationItem model.appMode FrameRules "Frame Alarm"
      ]
    ]

navigationItem : AppMode -> AppMode -> String -> Html ViewMsg
navigationItem current target title =
  li [ classList [ ("selected", current == target) ] ]
    [ input
      [ type_ "radio"
      , Html.Attributes.name "navigation"
      , id (title ++ "-navigation")
      , value title
      , onCheck (\_ -> Navigate target)
      , checked (current == target)
      ] []
    , label [ for (title ++ "-navigation") ] [ text title ]
    ]

displayAudioRules model =
  div [ id "audio-rules" ]
    [ displayRuleSet model model.currentScene.sources model.ruleSet
    ]

displayFrameRules model =
  div [ id "frame-rules" ]
    [ div [ (ruleClasses
              (model.droppedFrameRate > 0)
              (model.droppedFrameRate > 0.2)
              False
            )
          ]
      [ text <| "Dropped Frames " ++ (toPercent model.droppedFrameRate)
      ]
    ]

displaySelectVideo model =
  let scene = model.currentScene in
  div [ id "select-video" ]
    [ h2 [] [ text scene.name ]
    , p [ class "heading-note" ]
      [ text "Only sources from the active scene are shown." ]
    , p [ class "instructions" ] [ text
    """Select video source to copy audio rules to.
    The first visible source will have it's audio rules checked. """ ]
    , button [ onClick Cancel ] [ text "Cancel" ]
    , table [ class "source-list" ]
      <| List.map (displaySourceForSelect SelectVideoSource)
      <| List.filter (noCurrentRule model.ruleSet)
      <| List.filter mightBeVideoSource
      <| scene.sources
    ]

noCurrentRule : RuleSet -> Source -> Bool
noCurrentRule ruleSet source =
  Nothing == (RuleSet.get (VideoState source.name Visible) ruleSet)

displaySelectAudio model operator audioStates =
  let sources = Dict.values model.allSources in
  div [ id "select-audio" ]
    [ div []
      [ audioGroup "Any" (operator == Any)
        (SelectAudioMode Any)
      , audioGroup "All" (operator == All)
        (SelectAudioMode All)
      ]
    , sources
      |> List.filter mightBeAudioSource
      |> (\ss -> List.append ss (missingAudioSources audioStates ss))
      |> List.map (displayAudioSourceChoice audioStates)
      |> table [ class "source-list" ]
    ]

targetValue : Json.Decode.Decoder a -> (a -> ViewMsg) -> Json.Decode.Decoder ViewMsg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

int : Json.Decode.Decoder Int
int =
  Json.Decode.string
    |> Json.Decode.andThen (\text ->
      if validNumber text then
        Json.Decode.succeed <| getNumber text
      else
        Json.Decode.fail "not an integer"
      )

getNumber : String -> Int
getNumber s =
  String.toInt s |> Result.withDefault 0

validNumber : String -> Bool
validNumber value =
  Regex.contains (regex "^\\d+$") value

alarmStatus model =
  div [ class "alarm-status" ]
    [ label [] [ text "Alarm Status " ]
    , violated model.time model.alarm
    ]

violated : Int -> Alarm -> Html ViewMsg
violated time alarm =
  case alarm of
    Silent ->
      alarmTime 1 0
    Violation start timeout ->
      alarmTime timeout (time - start)
    Alarming start ->
      alarmTime 1 (time - start)

alarmTime : Int -> Int -> Html ViewMsg
alarmTime max val =
  span []
    [ progress
      [ Html.Attributes.max <| toString max
      , value <| toString val
      ] []
    , text " "
    , if val > 0 then text <| toString val else text ""
    ]

audioPlaying : AlarmRepeat -> Bool
audioPlaying alarm =
  case alarm of
    Notice _ -> True
    Rest _ -> False

displayConnectionStatus : ConnectionStatus -> Html ViewMsg
displayConnectionStatus connected =
  div [ class "connection-status" ]
    [ case connected of
      Disconnected ->
        div [ class "disconnected" ] [ text "Disconnected" ]
      Connecting ->
        div [ class "connecting" ]
          [ text "Requires "
          , a [ href "https://obsproject.com/forum/resources/obs-websocket-remote-control-of-obs-studio-made-easy.466/" ]
            [ text "OBS Websockets" ]
          , text " tested on 4.2.0"
          ]
      Connected version ->
        div [ class "connected" ]
          [ button [ id "logout", onClick LogOut ] [ text "log out" ]
          , text ("Connected (not authenticated) v" ++ version)
          , text " "
          ]
      AuthRequired version _ ->
        input
          [ type_ "password"
          , on "change" <| targetValue Json.Decode.string SetPassword
          , placeholder "OBS Websockets password"
          ] []
      LoggingIn version ->
        div [ class "logging-in" ] [ text "Logging In..." ]
      Authenticated version->
        div [ class "authenticated" ]
          [ button [ id "logout", onClick LogOut ] [ text "log out" ]
          , text ("OBS-Websocket v" ++ version)
          , text " "
          ]
   ]

--displayRuleSet : Model -> List Source -> RuleSet -> Html ViewMsg
displayRuleSet model sources ruleSet =
  let
    activeVideoState = RuleSet.activeVideoState sources ruleSet
    sourceOrder = sources
      |> List.indexedMap (\index source -> (source.name, index))
      |> Dict.fromList
    ruleSourceOrder = ruleSet
      |> RuleSet.toList
      |> List.map (\((VideoState name _), _) -> name)
      |> List.sort
      |> List.indexedMap (\index name -> (name, index + 1000))
      |> Dict.fromList
    copyable = not <| List.isEmpty sources
    hintCopy = (List.isEmpty (RuleSet.toList ruleSet) && copyable)
    hintAudio = List.all (\audio -> audio == RuleSet.defaultAudio) (RuleSet.audioRules ruleSet) && copyable
  in
  div []
    [ ruleSet
      |> RuleSet.toList
      |> List.sortBy (\((VideoState name _), _) ->
        Dict.get name sourceOrder
          |> Maybe.withDefault (Dict.get name ruleSourceOrder
            |> Maybe.withDefault 2000)
      )
      |> List.map (\rule ->
          let
            videoState = Tuple.first rule
            (VideoState name _) = videoState
          in
          lazy3 displayRule
          copyable
          (ruleClasses 
            ((Just videoState) == activeVideoState)
            (checkRule sources rule)
            (not <| Dict.member name sourceOrder)
          )
          rule
        )
      |> (flip List.append)
        [ lazy3 displayDefaultRule
          copyable
          (ruleClasses
            (Nothing == activeVideoState)
            ((Nothing == activeVideoState) && (checkAudioRule sources (RuleSet.default ruleSet)))
            False
          )
          (RuleSet.default ruleSet) ]
      |> (flip List.append)
        (if hintCopy then
          [ tr [ class "hint" ]
            [ th [ colspan 4 ] [ text "Start by copying audio rules to a source you want alarms for, such as BRB or Starting Soon." ]
            , th [] [ icon "arrow-up" ]
            ]
          ]
        else if hintAudio then
          [ tr [ class "hint" ]
            [ th [ colspan 2 ] []
            , th [ colspan 3 ] [ icon "arrow-up", text " Select the mics you use and what state they should alarm in." ]
            ]
          ]
        else
          []
        )
      |> List.append
        [ tr []
          [ th [ class "delete" ] [ text "Del" ]
          , th [] [ text "Video Source" ]
          , th [] [ text "Audio Status" ]
          , th [] [ text "Seconds" ]
          , th [] [ text "Copy" ]
          ]
        ]
      |> List.append
        [ tr [ class "hint" ]
          [ th [ colspan 2 ] [ text "Alarm if source visible" ]
          , th [] [ text "and mics are in this state" ]
          , th [ colspan 2] [ text "for _ seconds" ]
          ]
        ]
      |> table [ class "rules" ]
    ]

toPercent : Float -> String
toPercent f =
  (f * 100)
    |> toString
    |> String.left 4
    |> (flip (++) "%")

checkRule : List Source -> (VideoState, AudioRule) -> Bool
checkRule sources (video, audio) =
  (checkVideoState sources video) && (checkAudioRule sources audio)

displaySourceForSelect : (String -> ViewMsg) -> Source -> Html ViewMsg
displaySourceForSelect tagger source =
  tr
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    , onClick (tagger source.name)
    ]
    [ td [ class "icon-column" ] [ renderStatus source.render ]
    , td [ class "icon-column" ] [ audioStatus source.audio ]
    , td [] [ text source.name ]
    , td [] [ em [] [ text source.type_ ] ]
    ]

displayAudioSourceChoice : List AudioState -> Source -> Html ViewMsg
displayAudioSourceChoice audioStates source =
  let status = audioStates
    |> List.filterMap (matchingAudioStatus source.name)
    |> List.head
  in
  tr
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    ]
    [ td []
      [ input
        [ type_ "checkbox"
        , Html.Attributes.name (source.name ++ "-selected")
        , id (source.name ++ "-selected")
        , value "selected"
        , onCheck (\_ -> (SelectAudioSource source.name))
        , checked (status /= Nothing)
        ] []
      ]
    , td [ class "icon-column" ] [ renderStatus source.render ]
    , td [ class "icon-column" ]
      ( case status of
          Just audio ->
            [ input
              [ type_ "checkbox"
              , Html.Attributes.name (source.name ++ "-live")
              , id (source.name ++ "-live")
              , value "live"
              , onCheck (\_ -> (SelectAudioStatus source.name))
              , checked (status == Just Live)
              ] []
            , label [ for (source.name ++ "-live") ] [ audioStatus audio ]
            ]
          Nothing -> [ text "" ]
      )
    , td []
      [ label [ for (source.name ++ "-selected") ]
        [ text source.name ]
      ]
    , td [] [ em [] [ text source.type_ ] ]
    ]

matchingAudioStatus : String -> AudioState -> Maybe Audio
matchingAudioStatus sourceName (AudioState name status) =
  if name == sourceName then Just status else Nothing

missingAudioSources : List AudioState -> List Source -> List Source
missingAudioSources audioStates sources =
  let sourceNames = List.map .name sources in
  audioStates
    |> List.map (\(AudioState name _) -> name)
    |> List.filter (\name -> not <| List.member name sourceNames)
    |> List.map (\name -> 
      { name = name
      , render = Hidden
      , type_ = "missing audio source"
      , volume = 1.0
      , audio = Muted
      })

renderStatus : Render -> Html ViewMsg
renderStatus render =
  case render of
    Visible -> span [ class "video" ] [ icon "eye" ]
    Hidden -> span [ class "video" ] [ icon "eye-blocked" ]

audioStatus : Audio -> Html ViewMsg
audioStatus audio =
  case audio of
    Muted -> span [ class "audio muted" ] [ icon "volume-mute2" ]
    Live -> span [ class "audio live" ] [ icon "volume-medium" ]

displayRule : Bool -> Attribute ViewMsg -> (VideoState, AudioRule) -> Html ViewMsg
displayRule copyable classes (video, audio) =
  tr [ classes ]
    <| List.append
      [ td [ class "delete" ] [ button [ onClick (RemoveRule video) ] [ icon "bin" ] ]
      , (displayVideoRule video)
      ]
      (displayAudioRule copyable (VideoKey video) audio)

displayDefaultRule : Bool -> Attribute ViewMsg -> AudioRule -> Html ViewMsg
displayDefaultRule copyable classes audioRule =
  tr [ classes ]
    <| List.append
      [ td [ class "delete" ] []
      , td [] [ text "default " ]
      ]
      (displayAudioRule copyable DefaultKey audioRule)

ruleClasses : Bool -> Bool -> Bool -> Attribute ViewMsg
ruleClasses active violation missing =
  classList
    [ ("active", active)
    , ("violation", violation)
    , ("missing", missing)
    ]

displayVideoRule : VideoState -> Html ViewMsg
displayVideoRule videoState =
  case videoState of 
    VideoState sourceName render ->
      td []
        [ renderStatus render
        , text " "
        , text sourceName
        ]

displayAudioRule : Bool -> RuleKey -> AudioRule -> List (Html ViewMsg)
displayAudioRule copyable key (AudioRule operator states timeout) =
  [ td
    []
    [ button
      [ onClick <| SelectRuleAudioRule key ]
      [ div [ class "audio-status" ]
        [ div [ class "edit" ] [ icon "pencil" ]
        , div [ class "operator" ] [ text <| toString operator ]
        , ul [ class "audio-states" ]
          <| List.map (\e -> li [] [e])
          <| List.map displayAudioState states
        ]
      ]
    ]
  , td []
    [ input
      [ value <| toString timeout
      , type_ "number"
      , Html.Attributes.min "0"
      , on "change" <| targetValue int (SetTimeout key)
      , class "timeout"
      ] []
    ]
  , td []
    [ button
      [ onClick (CopyRule key), disabled (not copyable) ]
      [ icon "copy" ]
    ]
  ]

displayAudioState : AudioState -> Html ViewMsg
displayAudioState (AudioState sourceName audio) =
  div []
    [ text sourceName
    , text " "
    , audioStatus audio
    ]

audioGroup : String -> Bool -> ViewMsg -> Html ViewMsg
audioGroup name isSelected msg =
  input
    [ type_ "button"
    , Html.Attributes.name ("audio-group-" ++ name)
    , id ("audio-group-" ++ name)
    , value name
    , onClick msg
    , classList
      [ ("current-mode", isSelected)
      , ("audio-mode", True)
      ]
    ] []

icon : String -> Html ViewMsg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("#icon-"++name) ] [] ]
