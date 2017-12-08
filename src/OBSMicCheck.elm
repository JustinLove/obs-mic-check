module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..), AppMode(..))
import OBSWebSocket
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData)
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources)
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Alarm(..))

import Html
import WebSocket
import Json.Decode
import Json.Encode
import Set

obsAddress = "ws://localhost:4444"

main =
  Html.program
    { init = init
    , view = (\model -> Html.map View (view model))
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type ConnectionStatus
 = NotConnected
 | Connected String
 | Authenticated String

type alias Model =
  { connected : ConnectionStatus
  , time : Int
  , password : String
  , currentScene : Scene
  , specialSources : SpecialSources
  , ruleSet : RuleSet
  , activeAudioRule : AudioRule
  , alarm : Alarm
  , appMode : AppMode
  }

init : (Model, Cmd Msg)
init =
  (makeModel, Cmd.none)

allMics audio =
  [ (AudioState "Podcaster - audio" audio)
  , (AudioState "Podcaster - Stepmania" audio)
  ]

defaultAudio = AudioRule (AllAudio (allMics Muted)) 5

makeModel : Model
makeModel =
  Model
    NotConnected
    0
    ""
    { name = "-", sources = []}
    (SpecialSources Nothing Nothing Nothing Nothing Nothing)
    ( RuleSet.empty defaultAudio
      |> RuleSet.insert 
        (VideoState "BRB - text 2" Visible) 
        (AudioRule (AnyAudio (allMics Live)) 5)
      |> RuleSet.insert 
        (VideoState "Starting soon - text" Visible) 
        (AudioRule (AnyAudio (allMics Live)) 5)
      |> RuleSet.insert 
        (VideoState "Stream over - text" Visible) 
        (AudioRule (AnyAudio (allMics Live)) 60)
    )
    defaultAudio
    Silent
    Status

-- UPDATE

type Msg
  = OBS (Result String Message)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
      ({model | password = word}, attemptToConnect)
    View Connect ->
      (model, attemptToConnect)
    View SelectConfig ->
      ( { model | appMode = case model.appMode of 
          Status -> Config
          _ -> Status
        }
      , Cmd.none
      )
    View (SelectVideoRender videoState) ->
      let (VideoState name render) = videoState in
      ( updateActive { model
        | ruleSet = mapRuleKey videoState (VideoState name (toggleRender render)) model.ruleSet
        }
      , Cmd.none)
    View (SelectRuleVideoName videoState) ->
      ({model | appMode = SelectVideo videoState}, Cmd.none)
    View (SelectVideoSource source) ->
      case model.appMode of
        SelectVideo videoState ->
          let (VideoState _ render) = videoState in
          ( updateActive { model
            | ruleSet = mapRuleKey videoState (VideoState source render) model.ruleSet
            , appMode = Status
            }
          , Cmd.none)
        _ -> (model, Cmd.none)
    View (SelectAudioRule key) ->
      ({model | appMode = SelectAudio key}, Cmd.none)

updateResponse : ResponseData -> Model -> (Model, Cmd Msg)
updateResponse response model =
  case response of
    Response.GetVersion version ->
      ( { model | connected = Connected version.obsWebsocketVersion}
      , obsSend <| Request.getAuthRequired
      )
    Response.AuthRequired challenge ->
      ( model
      , obsSend <| Request.authenticate (OBSWebSocket.authenticate model.password challenge.salt challenge.challenge)
      )
    Response.AuthNotRequired ->
      authenticated model
    Response.Authenticate ->
      authenticated model
    Response.CurrentScene scene ->
      updateSources model scene model.specialSources
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
      let _ = Debug.log "status" status in
      if model.connected == NotConnected then
        (model, attemptToConnect)
      else
        if status.streaming then
          ( checkAlarms { model | time = status.totalStreamTime }
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
  ( { model | connected = authenticatedStatus model.connected}
  , refreshScene
  )

refreshScene : Cmd Msg
refreshScene =
  Cmd.batch
    [ obsSend <| Request.getCurrentScene
    , obsSend <| Request.getSpecialSources
    ]

authenticatedStatus : ConnectionStatus -> ConnectionStatus
authenticatedStatus connected =
  case connected of
    NotConnected ->
      Authenticated "-"
    Connected version->
      Authenticated version 
    Authenticated version->
      Authenticated version 
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
  let scenePlus = addSpecialSources specialSources scene in
  ( updateActive { model
    | currentScene = scenePlus
    , specialSources = specialSources
    }
  , scenePlus.sources
    |> List.map (.name >> Request.getMute >> obsSend)
    |> Cmd.batch
  )

addSpecialSources : SpecialSources -> Scene -> Scene
addSpecialSources specialSources scene =
  { scene | sources =
    scene.sources
      |> List.map .name
      |> Set.fromList
      |> Set.diff (specialSourceNames specialSources |> Set.fromList)
      |> Set.toList
      |> List.map (\name -> Source name Hidden "special-source" 1.0 Live)
      |> List.append scene.sources
  }

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
  in
  { model
  | activeAudioRule = newActive
  , alarm =
    if model.activeAudioRule == newActive then
      model.alarm
    else
      if RuleSet.checkAudioRule sources newActive then
        Violation model.time
      else
        Silent
  }

checkAlarms : Model -> Model
checkAlarms model =
  let
    sources = model.currentScene.sources
    violation = RuleSet.checkAudioRule sources model.activeAudioRule
  in
  { model | alarm =
    case (model.alarm, violation) of
      (_, False) -> Silent
      (Silent, True) -> Violation model.time
      (Alarming start, True) -> Alarming start
      (Violation start, True) ->
        checkTimeout start model.time model.activeAudioRule
  }

checkTimeout : Int -> Int -> AudioRule -> Alarm
checkTimeout start time (AudioRule _ timeout) =
  if time - start > timeout then
    Alarming start
  else
    Violation start

mapRuleKey : VideoState -> VideoState -> RuleSet -> RuleSet
mapRuleKey key replacement ruleSet =
  case RuleSet.get key ruleSet of
    Just audio ->
      ruleSet
        |> RuleSet.remove key
        |> RuleSet.insert replacement audio
    Nothing -> ruleSet

toggleRender : Render -> Render
toggleRender render =
  case render of
    Visible -> Hidden
    Hidden -> Visible

obsSend : Json.Encode.Value -> Cmd Msg
obsSend message =
  WebSocket.send obsAddress (Json.Encode.encode 0 message)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen obsAddress receiveMessage

receiveMessage : String -> Msg
receiveMessage =
  OBS << Json.Decode.decodeString message
