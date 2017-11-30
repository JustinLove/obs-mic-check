module OBSWebSocket.Message exposing (..)

import OBSWebSocket.Response as Response exposing (..)
import OBSWebSocket.Event as Event exposing (..)

import Json.Decode exposing (..)

type Message
  = Response String ResponseData
  | Event EventData

responseWrapper : Decoder ResponseData -> Decoder Message
responseWrapper =
  map2 Response
    (field "message-id" string)

eventWrapper : Decoder EventData -> Decoder Message
eventWrapper =
  map Event

message : Decoder Message
message =
  oneOf
    [ Response.getVersion |> responseWrapper
    , Event.streamStatus |> eventWrapper
    ]
