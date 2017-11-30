module OBSWebSocket.Event exposing (..)

import Json.Decode exposing (..)

type EventData
  = StreamStatus StreamStatusStruct

type alias StreamStatusStruct =
  { streaming : Bool
  , recording : Bool
  }

streamStatus : Decoder EventData
streamStatus =
  map2 StreamStatusStruct
    (field "streaming" bool)
    (field "streaming" bool)
  |> map StreamStatus 
