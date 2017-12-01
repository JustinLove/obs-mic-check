module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))
import OBSWebSocket
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData, Scene, Source)
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)

import Html
import WebSocket
import Json.Decode
import Json.Encode

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
  }

init : (Model, Cmd Msg)
init =
  (Model NotConnected "password" { name = "-", sources = []}, Cmd.none)

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
      ({ model | currentScene = scene }, Cmd.none)
    OBS (Ok (Event (Event.StreamStatus status))) ->
      let _ = Debug.log "status" status in
      if model.connected == NotConnected then
        (model, attemptToConnect)
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
  , obsSend <| Request.getCurrentScene
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
