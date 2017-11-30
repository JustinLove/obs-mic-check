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

obsAddress = "ws://localhost:4444"

main =
  Html.program
    { init = init
    , view = (\model -> Html.map View (view model))
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  {
  }

init : (Model, Cmd Msg)
init =
  ({}, Cmd.none)

-- UPDATE

type Msg
  = OBS (Result String Message)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OBS (Ok (Response id (Response.GetVersion version))) ->
      let _ = Debug.log "version" version in
      (model, Cmd.none)
    OBS (Ok (Event (Event.StreamStatus status))) ->
      let _ = Debug.log "status" status in
      (model, Cmd.none)
    OBS (Err message) ->
      let _ = Debug.log "decode error" message in
      (model, Cmd.none)
    View Test ->
      (model, WebSocket.send obsAddress (Json.Encode.encode 0 Request.getVersion))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen obsAddress receiveMessage

receiveMessage : String -> Msg
receiveMessage =
  OBS << Json.Decode.decodeString message
