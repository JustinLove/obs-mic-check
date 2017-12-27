module View exposing (view, ViewMsg(..))

import Model exposing (Model, AppMode(..), RuleKey(..), ConnectionStatus(..))
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), Challenge, StatusReport, mightBeVideoSource, mightBeAudioSource)
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..), checkVideoState, checkAudioRule)
import Alarm exposing (Alarm(..), AlarmRepeat(..), isAlarming)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Plot exposing (defaultSeriesPlotCustomizations)
import Json.Decode
import Dict
import Regex exposing (regex)

type ViewMsg
  = None
  | SetObsHost String
  | SetObsPort Int
  | Connect
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
  | SetFrameSampleWindow Int
  | SetFrameAlarmLevel Float

-- VIEW

view : Model -> Html ViewMsg
view model =
  div []
    [ case model.connected of
      Disconnected ->
        connectionView model
      Connecting ->
        connectionView model
      Connected version ->
        connectionView model
      AuthRequired version _ ->
        input
          [ type_ "password"
          , on "change" <| targetValue Json.Decode.string SetPassword
          , placeholder "OBS Websockets password"
          ] []
      LoggingIn version ->
        div [ class "logging-in" ] [ text "Logging In..." ]
      Authenticated version->
        applicationView model
   ]

connectionView : Model -> Html ViewMsg
connectionView model =
  div [ id "connection-view" ]
    [ connectionConfigView model
    , aboutView model
    ]

connectionConfigView : Model -> Html ViewMsg
connectionConfigView model =
  div [ id "connection-config" ]
    [ h3 [] [ text "Connect" ]
    , div [ class "setting config-obs-host" ]
      [ label [] [ text "OBS Hostname" ]
      , input
        [ value model.obsHost
        , type_ "text"
        , on "change" <| targetValue Json.Decode.string SetObsHost
        ] []
      ]
    , div [ class "setting config-obs-port" ]
      [ label [] [ text "OBS Port" ]
      , input
        [ value <| toString model.obsPort
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "65535"
        , on "change" <| targetValue int SetObsPort
        ] []
      ]
    , button [ id "connect", onClick Connect ] [ text "Connect" ]
    ]

aboutView : Model -> Html ViewMsg
aboutView model =
  div [ id "about" ]
    [ h2 [] [ text "OBS Mic-Check" ]
    , p []
      [ text "Check that "
      , a [ href "https://obsproject.com/" ] [ text "OBS Studio" ]
      , text """ audio state and active video sources are synchronized, e.g., when "BRB" is showing, mic should be off, and vice-versa. Also includes alarms for dropped frames.""" ]
    , h3 [] [ text "Requirements" ]
    , p []
      [ text "Requires "
      , a [ href "https://obsproject.com/forum/resources/obs-websocket-remote-control-of-obs-studio-made-easy.466/" ]
        [ text "OBS Websockets" ]
      , text " tested on 4.2.0"
      ]
    , h3 [] [ text "Contact" ]
    , p []
      [ a [ href "https://github.com/JustinLove/obs-mic-check" ]
        [ text "Github" ]
      , text " "
      , a [ href "https://twitter.com/wondible" ]
        [ text "@wondible" ]
      ]
    ]

applicationView : Model -> Html ViewMsg
applicationView model =
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

displayHeader : Model -> Html ViewMsg
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

displayNavigation : Model -> Html ViewMsg
displayNavigation model =
  let
    activeVideoState = RuleSet.activeVideoState model.currentScene.sources model.ruleSet
    audioTitle = case activeVideoState of
      Just (VideoState name _) -> name
      Nothing -> "Audio Alarms"
  in
  nav []
    [ ul []
      [ navigationItem model.appMode AudioRules "audio-alarms"
        audioTitle
        (iconForAlarm model.audioAlarm)
      , navigationItem model.appMode FrameRules "frame-alarm"
        ("Dropped Frames " ++ (toPercent model.droppedFrameRate))
        (iconForAlarm model.frameAlarm)
      ]
    ]

navigationItem : AppMode -> AppMode -> String -> String -> Html ViewMsg -> Html ViewMsg
navigationItem current target itemId title status =
  li [ classList [ ("selected", current == target) ] ]
    [ input
      [ type_ "radio"
      , Html.Attributes.name "navigation"
      , id (itemId ++ "-navigation")
      , value title
      , onCheck (\_ -> Navigate target)
      , checked (current == target)
      ] []
    , label [ for (itemId ++ "-navigation") ]
      [ text title
      , text " "
      , status
      ]
    ]

displayAudioRules : Model -> Html ViewMsg
displayAudioRules model =
  div [ id "audio-rules" ]
    [ displayRuleSet model model.currentScene.sources model.ruleSet
    ]

displayFrameRules : Model -> Html ViewMsg
displayFrameRules model =
  div [ id "frame-rules" ]
    [ p [ (ruleClasses
              (model.droppedFrameRate > 0)
              (model.droppedFrameRate > model.frameAlarmLevel)
              False
            )
          ]
      [ text <| "Dropped Frames " ++ (toPercent model.droppedFrameRate)
      ]
    , lazy2 displayFrameParameters model.frameSampleWindow model.frameAlarmLevel
    , div [ class "chart" ] [ lazy displayFrameGraph model.recentStatus ]
    ]

displayFrameParameters : Int -> Float -> Html ViewMsg
displayFrameParameters frameSampleWindow frameAlarmLevel =
  div []
    [ p [ class "config-frame-sample-window" ]
      [ input
        [ value <| toString frameSampleWindow
        , type_ "number"
        , Html.Attributes.min "1"
        , on "change" <| targetValue int SetFrameSampleWindow
        ] []
      , text " "
      , label [] [ text "Sample Seconds" ]
      ]
    , p [ class "config-frame-alarm-level" ]
      [ input
        [ value <| toString (frameAlarmLevel * 100.0)
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        , step "0.1"
        , on "change" <| targetValue (Json.Decode.map ((*) 0.01) <| float) SetFrameAlarmLevel
        ] []
      , text " "
      , label [] [ text "Alarm Level %" ]
    ]
  ]

displayFrameGraph : List StatusReport -> Html ViewMsg
displayFrameGraph recentStatus =
   Plot.viewSeriesCustom
    { defaultSeriesPlotCustomizations
    | height = 200
    }
    [ Plot.line <| extractSeries .numTotalFrames Plot.clear
    , Plot.area <| extractSeries .numDroppedFrames Plot.diamond
    ]
    recentStatus

extractSeries : (StatusReport -> Int) -> (Float -> Float -> Plot.DataPoint msg)-> List StatusReport -> List (Plot.DataPoint msg)
extractSeries selector dataPoint recentStatus =
  let
    data = recentStatus
      |> List.map selector
    diffs = List.map2 (-) data (List.drop 1 data)
      |> List.map toFloat
    times = recentStatus
      |> List.map .totalStreamTime
      |> List.map toFloat
      |> List.drop 1
  in
    List.map2 dataPoint times diffs

displaySelectVideo : Model -> Html ViewMsg
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

displaySelectAudio : Model -> Operator -> List AudioState -> Html ViewMsg
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
      if validInt text then
        Json.Decode.succeed <| getInt text
      else
        Json.Decode.fail "not an integer"
      )

float : Json.Decode.Decoder Float
float =
  Json.Decode.string
    |> Json.Decode.andThen (\text ->
      if validFloat text then
        Json.Decode.succeed <| getFloat text
      else
        Json.Decode.fail "not a floating point number"
      )

getInt : String -> Int
getInt s =
  String.toInt s |> Result.withDefault 0

getFloat : String -> Float
getFloat s =
  String.toFloat s |> Result.withDefault 0

validInt : String -> Bool
validInt value =
  Regex.contains (regex "^\\d+$") value

validFloat : String -> Bool
validFloat value =
  Regex.contains (regex "^\\d+(\\.\\d+)?$") value

alarmStatus : Model -> Html ViewMsg
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

displayRuleSet : Model -> List Source -> RuleSet -> Html ViewMsg
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

iconForAlarm : Alarm -> Html ViewMsg
iconForAlarm alarm =
  case alarm of
    Silent ->
      text ""
    Violation _ _ ->
      icon "warning"
    Alarming _ ->
      icon "fire"

icon : String -> Html ViewMsg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("#icon-"++name) ] [] ]
