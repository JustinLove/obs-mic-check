module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))
import OBSWebSocket.Encode as Request
import OBSWebSocket.Decode as Response

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
  = OBS (Result String Response.GetVersion)
  | View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OBS (Ok value) ->
      let _ = Debug.log "got" value in
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
  OBS << (Json.Decode.decodeString Response.getVersion)
