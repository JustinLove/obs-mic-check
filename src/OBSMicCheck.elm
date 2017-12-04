module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))
import OBSWebSocket
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData)
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources)
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)
import AlarmRule exposing (RuleSet(..), AlarmRule(..), VideoRule(..), AudioRule(..))

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
  , password : String
  , currentScene : Scene
  , specialSources : SpecialSources
  , ruleSet : RuleSet
  , alarm : Maybe AudioRule
  }

init : (Model, Cmd Msg)
init =
  (makeModel, Cmd.none)

allMics audio =
  [ (AudioRule "Podcaster - audio" audio)
  , (AudioRule "Podcaster - Stepmania" audio)
  ]

makeModel : Model
makeModel =
  Model
    NotConnected
    ""
    { name = "-", sources = []}
    (SpecialSources Nothing Nothing Nothing Nothing Nothing)
    ( RuleSet (AllAudio (allMics Muted))
      [ AlarmRule
        (SourceRule "BRB - text 2" Visible) 
        (AnyAudio (allMics Live))
      , AlarmRule
        (SourceRule "Starting soon - text" Visible) 
        (AnyAudio (allMics Live))
      , AlarmRule
        (SourceRule "Stream over - text" Visible) 
        (AnyAudio (allMics Live))
      ]
    )
    Nothing

-- UPDATE

type Msg
  = OBS (Result String Message)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OBS (Ok (Response id (Response.GetVersion version))) ->
      ( { model | connected = Connected version.obsWebsocketVersion}
      , obsSend <| Request.getAuthRequired
      )
    OBS (Ok (Response id (Response.AuthRequired challenge))) ->
      ( model
      , obsSend <| Request.authenticate (OBSWebSocket.authenticate model.password challenge.salt challenge.challenge)
      )
    OBS (Ok (Response id (Response.AuthNotRequired))) ->
      authenticated model
    OBS (Ok (Response id (Response.Authenticate))) ->
      authenticated model
    OBS (Ok (Response id (Response.CurrentScene scene))) ->
      updateSources model scene model.specialSources
    OBS (Ok (Response id (Response.GetMuted sourceName audio))) ->
      ( checkAlarms {model | currentScene = setAudio model.currentScene sourceName audio}
      , Cmd.none
      )
    OBS (Ok (Response id (Response.GetSpecialSources sources))) ->
      updateSources model model.currentScene sources
    OBS (Ok (Event (Event.IgnoredEvent updateType))) ->
      (model, Cmd.none)
    OBS (Ok (Event (Event.SwitchScenes scene))) ->
      updateSources model scene model.specialSources
    OBS (Ok (Event (Event.SceneItemAdded sceneName sourceName))) ->
      (model, refreshScene)
    OBS (Ok (Event (Event.SceneItemRemoved sceneName sourceName))) ->
      (model, refreshScene)
    OBS (Ok (Event (Event.SceneItemVisibilityChanged sceneName sourceName render))) ->
      ( checkAlarms {model | currentScene = setRender model.currentScene sourceName render}
      , Cmd.none
      )
    OBS (Ok (Event (Event.StreamStatus status))) ->
      let _ = Debug.log "status" status in
      if model.connected == NotConnected then
        (model, attemptToConnect)
      else
        if status.streaming then
          (checkAlarms model
          , model.ruleSet
            |> AlarmRule.audioSourceNames
            |> List.map (Request.getMute >> obsSend)
            |> Cmd.batch
          )
        else
          (model, Cmd.none)
    OBS (Err message) ->
      let _ = Debug.log "decode error" message in
      (model, Cmd.none)
    View (SetPassword word) ->
      ({model | password = word}, attemptToConnect)
    View Connect ->
      (model, attemptToConnect)

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
  ( { model
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

checkAlarms : Model -> Model
checkAlarms model =
  { model | alarm =
    AlarmRule.alarmingRule model.currentScene.sources model.ruleSet
  }

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
