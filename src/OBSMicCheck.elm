module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))

import Html
import WebSocket
import Json.Decode

main =
  Html.program
    { init = init
    , view = view
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
  = OBS (Result String Json.Decode.Value)
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
    View None -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:4444" receiveMessage

receiveMessage : String -> Msg
receiveMessage =
  OBS << (Json.Decode.decodeString Json.Decode.value)
