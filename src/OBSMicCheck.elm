module OBSMicCheck exposing (..)

import Harbor exposing (..)
import Model exposing (..)
import Model.Encode
import Model.Decode
import View exposing (view, ViewMsg(..))
import OBSWebSocket
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData)
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources, StatusReport)
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..))
import Alarm exposing (Alarm(..), AlarmRepeat(..), mergeAlarms, updateAlarmState, isAlarming, updateRepeat)
import RuleSet.Encode
import RuleSet.Decode

import Html
import WebSocket
import Json.Decode
import Json.Encode
import Set
import Dict exposing (Dict)
import Process
import Task
import Time

obsAddress = "ws://localhost:4444"

main =
  Html.program
    { init = init
    , view = (\model -> Html.map View (view model))
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  ({ makeModel | connected = Connecting }, attemptToConnect)

-- UPDATE

type Msg
  = Loaded (Result String PersistenceModel)
  | AttemptToConnect
  | OBS (Result String Message)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded (Ok pm) ->
      ({model | ruleSet = pm.ruleSet}, Cmd.none)
    Loaded (Err message) ->
      let _ = Debug.log "load error" message in
      (model, Cmd.none)
    AttemptToConnect ->
      ({ model | connected = Connecting }, attemptToConnect)
    OBS (Ok (Response id response)) ->
      updateResponse response model
    OBS (Ok (Event event)) ->
      updateEvent event model
    OBS (Err message) ->
      let _ = Debug.log "decode error" message in
      (model, Cmd.none)
    View (None) ->
      (model, Cmd.none)
    View (SetPassword word) ->
      case model.connected of
        AuthRequired version challenge ->
          ( {model | connected = LoggingIn version }
          , obsSend <| Request.authenticate
            (OBSWebSocket.authenticate word challenge.salt challenge.challenge)
          )
        _ -> (model, Cmd.none)
    View LogOut -> 
      ( { makeModel | ruleSet = model.ruleSet }
      , Process.sleep 100
        |> Task.perform (\_ -> AttemptToConnect)
      )
    View Cancel ->
      ( { model | appMode = AudioRules }, Cmd.none )
    View (Navigate mode) ->
      ( { model | appMode = mode }, Cmd.none )
    View (SelectVideoSource source) ->
      case model.appMode of
        SelectVideo audioRule ->
          { model
          | ruleSet = RuleSet.insert (VideoState source Visible) audioRule model.ruleSet
          , appMode = AudioRules
          }
            |> updateActive
            |> persist
        _ -> (model, Cmd.none)
    View (SelectRuleAudioRule key) ->
      case key of
        VideoKey videoState ->
          case RuleSet.get videoState model.ruleSet of
            Just (AudioRule operator audioState _) ->
              ( { model
                | appMode = SelectAudio key operator audioState
                }
              , Cmd.none)
            Nothing ->
              (model, Cmd.none)
        DefaultKey ->
          let (AudioRule operator audioState _) = RuleSet.default model.ruleSet in
          ( { model
            | appMode = SelectAudio key operator audioState
            }
          , Cmd.none)
    View (SelectAudioSource sourceName) ->
      case model.appMode of
        SelectAudio ruleKey operator audioStates ->
          ( updateActive { model
            | appMode = SelectAudio ruleKey operator
              (toggleAudioSource sourceName audioStates)
            }
          , Cmd.none)
        _ -> (model, Cmd.none)
    View (SelectAudioStatus sourceName) ->
      case model.appMode of
        SelectAudio ruleKey operator audioStates ->
          ( updateActive { model
            | appMode = SelectAudio ruleKey operator
              (List.map (toggleAudioStatus sourceName) audioStates)
            }
          , Cmd.none)
        _ -> (model, Cmd.none)
    View (SelectAudioMode operator) ->
      case model.appMode of
        SelectAudio ruleKey _ audioStates ->
          { model
          | ruleSet = mapRuleValue ruleKey
            (\(AudioRule _ _ timeout) -> AudioRule operator audioStates timeout)
            model.ruleSet
          , appMode = AudioRules
          }
            |> updateActive
            |> persist
        _ -> (model, Cmd.none)
    View (SetTimeout ruleKey timeout) ->
      { model
      | ruleSet = mapRuleValue ruleKey
        (\(AudioRule operator state _) -> AudioRule operator state timeout)
        model.ruleSet
      }
      |> updateActive
      |> persist
    View (CopyRule key) ->
      case key of
        VideoKey videoState ->
          case RuleSet.get videoState model.ruleSet of
            Just audioRule ->
              ( {model | appMode = SelectVideo audioRule }
              , Cmd.none)
            Nothing ->
              (model, Cmd.none)
        DefaultKey ->
          ( {model | appMode = SelectVideo (RuleSet.default model.ruleSet) }
          , Cmd.none)
    View (RemoveRule videoState) ->
      { model
      | ruleSet = RuleSet.remove videoState model.ruleSet
      }
        |> updateActive
        |> persist
    View (SetFrameSampleWindow window) ->
      { model | frameSampleWindow = window }
        |> persist
    View (SetFrameAlarmLevel level) ->
      { model | frameAlarmLevel = min 1.0 level }
        |> persist

updateResponse : ResponseData -> Model -> (Model, Cmd Msg)
updateResponse response model =
  case response of
    Response.GetVersion version ->
      ( { model | connected = Connected version.obsWebsocketVersion}
      , obsSend <| Request.getAuthRequired
      )
    Response.AuthRequired challenge ->
      ( { model | connected = AuthRequired (connectionVersion model.connected) challenge }
      , Cmd.none
      )
    Response.AuthNotRequired ->
      authenticated model
    Response.Authenticate True ->
      authenticated model
    Response.Authenticate False ->
      ( { model | connected = Connected (connectionVersion model.connected) }
      , obsSend <| Request.getAuthRequired
      )
    Response.CurrentScene scene ->
      updateSources model scene model.specialSources
    Response.SceneList currentSceneName scenes ->
      ( { model
        | allSources =
          updateAllSources model.specialSources
            (List.concatMap .sources scenes)
        }
      , Cmd.none
      )
    Response.GetMuted sourceName audio ->
      ( checkAlarms {model | currentScene = setAudio model.currentScene sourceName audio}
      , Cmd.none
      )
    Response.GetSpecialSources sources ->
      updateSources model model.currentScene sources

updateEvent : EventData -> Model -> (Model, Cmd Msg)
updateEvent event model =
  case event of
    Event.IgnoredEvent updateType ->
      (model, Cmd.none)
    Event.SwitchScenes scene ->
      updateSources model scene model.specialSources
    Event.SceneItemAdded sceneName sourceName ->
      (model, refreshScene)
    Event.SceneItemRemoved sceneName sourceName ->
      (model, refreshScene)
    Event.SceneItemVisibilityChanged sceneName sourceName render ->
      ( updateActive {model | currentScene = setRender model.currentScene sourceName render}
      , Cmd.none
      )
    Event.StreamStatus status ->
      if model.connected == Disconnected then
        (model, attemptToConnect)
      else
        if status.streaming then
          ( checkAlarms
            { model
            | time = status.totalStreamTime
            , recentStatus = model.recentStatus
              |> (::) status
              |> List.take ((model.frameSampleWindow // 2) + 1)
                -- one extra so we get N differences
            }
          , model.ruleSet
            |> RuleSet.audioSourceNames
            |> List.map (Request.getMute >> obsSend)
            |> Cmd.batch
          )
        else
          (model, Cmd.none)

attemptToConnect : Cmd Msg
attemptToConnect =
  obsSend <| Request.getVersion

authenticated : Model -> (Model, Cmd Msg)
authenticated model =
  ( { model | connected = Authenticated <| connectionVersion model.connected}
  , refreshScene
  )

refreshScene : Cmd Msg
refreshScene =
  Cmd.batch
    [ obsSend <| Request.getCurrentScene
    , obsSend <| Request.getSceneList
    , obsSend <| Request.getSpecialSources
    ]

connectionVersion : ConnectionStatus -> String
connectionVersion connected =
  case connected of
    Disconnected ->
      "-"
    Connecting ->
      "-"
    Connected version ->
      version 
    AuthRequired version _ ->
      version 
    LoggingIn version ->
      version 
    Authenticated version->
      version 

setRender : Scene -> String -> Render -> Scene
setRender scene sourceName render =
  { scene | sources =
    List.map (\source ->
        if source.name == sourceName then
          { source | render = render }
        else
        source )
      scene.sources
  }

setAudio : Scene -> String -> Audio -> Scene
setAudio scene sourceName audio =
  { scene | sources =
    List.map (\source ->
        if source.name == sourceName then
          { source | audio = audio }
        else
        source )
      scene.sources
  }

updateSources : Model -> Scene -> SpecialSources -> (Model, Cmd Msg)
updateSources model scene specialSources =
  let
    scenePlus = { scene
      | sources = addSpecialSources specialSources scene.sources }
  in
  ( updateActive { model
    | currentScene = scenePlus
    , specialSources = specialSources
    , allSources = updateAllSources specialSources (Dict.values model.allSources)
    }
  , scenePlus.sources
    |> List.map (.name >> Request.getMute >> obsSend)
    |> Cmd.batch
  )

updateAllSources : SpecialSources -> List Source -> Dict String Source
updateAllSources specialSources sources =
  addSpecialSources specialSources sources
    |> List.map (\s -> (s.name, s))
    |> Dict.fromList

addSpecialSources : SpecialSources -> List Source -> List Source
addSpecialSources specialSources sources =
  sources
    |> List.map .name
    |> Set.fromList
    |> Set.diff (specialSourceNames specialSources |> Set.fromList)
    |> Set.toList
    |> List.map (\name -> Source name Hidden "special-source" 1.0 Live)
    |> List.append sources

specialSourceNames : SpecialSources -> List String
specialSourceNames sources = 
  List.filterMap identity
    [ sources.desktop1
    , sources.desktop2
    , sources.mic1
    , sources.mic2
    , sources.mic3
    ]

updateActive : Model -> Model
updateActive model =
  let
    sources = model.currentScene.sources
    newActive = RuleSet.activeAudioRule sources model.ruleSet
    (AudioRule _ _ timeout) = newActive
  in
  { model
  | activeAudioRule = newActive
  , audioAlarm =
    if model.activeAudioRule == newActive then
      model.audioAlarm
    else
      if RuleSet.checkAudioRule sources newActive then
        Violation model.time timeout
      else
        Silent
  }

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveModel model)

checkAlarms : Model -> Model
checkAlarms model =
  let
    m2 = checkAudioAlarms <| checkFrameAlarms model
    alarm = mergeAlarms m2.frameAlarm m2.audioAlarm
  in
    { m2
    | alarm = alarm
    , alarmRepeat =
      if isAlarming alarm then
        updateRepeat 2 58 m2.time m2.alarmRepeat
      else
        Rest 0
    }

checkAudioAlarms : Model -> Model
checkAudioAlarms model =
  let
    sources = model.currentScene.sources
    audioViolation = RuleSet.checkAudioRule sources model.activeAudioRule
    (AudioRule _ _ timeout) = model.activeAudioRule
  in
    { model
    | audioAlarm = updateAlarmState model.time timeout audioViolation model.audioAlarm
    }

checkFrameAlarms : Model -> Model
checkFrameAlarms model =
  let
    dropped = droppedFrameRate model
    frameViolation = dropped > model.frameAlarmLevel
  in
    { model
    | droppedFrameRate = dropped
    , frameAlarm = updateAlarmState model.time 0 frameViolation model.frameAlarm
    }

droppedFrameRate : Model -> Float
droppedFrameRate model =
  let
    dropped = model.recentStatus
      |> List.map .numDroppedFrames
      |> sampleDifference
      |> toFloat
    total = model.recentStatus
      |> List.map .numTotalFrames
      |> sampleDifference
      |> toFloat
  in
    if total == 0.0 then
       0.0
    else
      (dropped / total)

sampleDifference : List Int -> Int
sampleDifference list =
  let
    length = List.length list
    newest = list
      |> List.head |> Maybe.withDefault 0
    oldest = list |> List.drop (length - 1)
      |> List.head |> Maybe.withDefault 0
  in
    newest - oldest

mapRuleValue : RuleKey -> (AudioRule -> AudioRule) -> RuleSet -> RuleSet
mapRuleValue key f ruleSet =
  case key of
    VideoKey videoState ->
      case RuleSet.get videoState ruleSet of
        Just audio ->
          ruleSet
            |> RuleSet.insert videoState (f audio)
        Nothing -> ruleSet
    DefaultKey ->
      RuleSet.mapDefault f ruleSet

toggleAudioSource : String -> List AudioState -> List AudioState
toggleAudioSource toggle states =
  let
    (matched, rest) = List.partition
      (\(AudioState name _) -> name == toggle)
      states
  in
    if List.isEmpty matched then
      (AudioState toggle Live) :: states
    else
      rest

toggleAudioStatus : String -> AudioState -> AudioState
toggleAudioStatus toggle state =
  case state of
    AudioState name status ->
      if name == toggle then
        AudioState name (toggleAudio status)
      else
        state

toggleAudio : Audio -> Audio
toggleAudio audio =
  case audio of
    Live -> Muted
    Muted -> Live

obsSend : Json.Encode.Value -> Cmd Msg
obsSend message =
  WebSocket.send obsAddress (Json.Encode.encode 0 message)

saveModel : Model -> Cmd Msg
saveModel model =
  { ruleSet = model.ruleSet
  }
    |> Model.Encode.persistenceModel
    |> Json.Encode.encode 0
    |> save

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch 
    [ if model.connected == Disconnected then
        Sub.none
      else
        WebSocket.listen obsAddress receiveMessage
    , if model.connected == Connecting then
        Time.every (10 * 1000) (\_ -> AttemptToConnect)
      else
        Sub.none
    , loaded (Loaded << Json.Decode.decodeString Model.Decode.persistenceModel)
    ]

receiveMessage : String -> Msg
receiveMessage =
  OBS << Json.Decode.decodeString message
