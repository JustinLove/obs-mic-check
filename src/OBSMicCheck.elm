module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))
import OBSWebSocket.Request as Request
import OBSWebSocket.Response as Response exposing (ResponseData)
import OBSWebSocket.Event as Event exposing (EventData)
import OBSWebSocket.Message as Message exposing (..)

import Html
import WebSocket
import Json.Decode
import Json.Encode
import Crypto.Hash exposing (sha256)
import Base64

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
  }

init : (Model, Cmd Msg)
init =
  (Model NotConnected "password", Cmd.none)

-- UPDATE

type Msg
  = OBS (Result String Message)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OBS (Ok (Response id (Response.GetVersion version))) ->
      ( { model | connected = Connected version.obsWebsocketVersion}
      , WebSocket.send obsAddress (Json.Encode.encode 0 Request.getAuthRequired)
      )
    OBS (Ok (Response id (Response.AuthRequired challenge))) ->
      let
        _ = Debug.log "challenge" challenge
        stuff = model.password ++ challenge.salt
        base64Secret = sha256 stuff |> Base64.encode
        more = base64Secret ++ challenge.challenge
        authResponse = sha256 more |> Base64.encode
      in
      (model, Cmd.none)
    OBS (Ok (Response id (Response.AuthNotRequired))) ->
      ( { model | connected = authenticatedStatus model.connected}
      , Cmd.none)
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
  WebSocket.send obsAddress (Json.Encode.encode 0 Request.getVersion)

authenticatedStatus : ConnectionStatus -> ConnectionStatus
authenticatedStatus connected =
  case connected of
    NotConnected ->
      Authenticated "-"
    Connected version->
      Authenticated version 
    Authenticated version->
      Authenticated version 

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen obsAddress receiveMessage

receiveMessage : String -> Msg
receiveMessage =
  OBS << Json.Decode.decodeString message
