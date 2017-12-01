module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))
import OBSWebSocket
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData)
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..))
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)
import AlarmRule exposing (AlarmRule)

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
  , rules : List AlarmRule
  , alarms : List AlarmRule
  }

init : (Model, Cmd Msg)
init =
  (makeModel, Cmd.none)

makeModel : Model
makeModel =
  Model
    NotConnected
    ""
    { name = "-", sources = []}
    [ AlarmRule "BRB - text 2" Visible "Podcaster - audio" Live
    , AlarmRule "BRB - text 2" Hidden "Podcaster - audio" Muted
    ]
    []

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
      ( { model | currentScene = scene }
      , scene.sources
        |> List.map (.name >> Request.getMute >> obsSend)
        |> Cmd.batch
      )
    OBS (Ok (Response id (Response.GetMuted sourceName audio))) ->
      ( checkAlarms {model | currentScene = setAudio model.currentScene sourceName audio}
      , Cmd.none
      )
    OBS (Ok (Response id (Response.GetSpecialSources sources))) ->
      let _ = Debug.log "special" sources in
      (model, Cmd.none)
    OBS (Ok (Event (Event.StreamStatus status))) ->
      let _ = Debug.log "status" status in
      if model.connected == NotConnected then
        (model, attemptToConnect)
      else
        if status.streaming then
          (checkAlarms model
          , model.rules
            |> List.map .audioSourceName
            |> Set.fromList
            |> Set.toList
            |> List.map (Request.getMute >> obsSend)
            |> Cmd.batch
          )
        else
          (model, Cmd.none)
    OBS (Ok (Event (Event.SceneItemVisibilityChanged sceneName sourceName render))) ->
      ( checkAlarms {model | currentScene = setRender model.currentScene sourceName render}
      , Cmd.none
      )
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
  , Cmd.batch
    [ obsSend <| Request.getCurrentScene
    , obsSend <| Request.getSpecialSources
    ]
  )

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

checkAlarms : Model -> Model
checkAlarms model =
  { model | alarms =
    List.filter (AlarmRule.checkRule model.currentScene.sources) model.rules
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
