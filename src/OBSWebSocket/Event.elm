module OBSWebSocket.Event exposing (..)

import OBSWebSocket.Data exposing (..)

import Json.Decode exposing (..)

type EventData
  = StreamStatus StreamStatusStruct
  | SceneItemVisibilityChanged String String Render

event : Decoder EventData
event =
  oneOf
    [ streamStatus
    , sceneItemVisibilityChanged
    ]

type alias StreamStatusStruct =
  { streaming : Bool
  , recording : Bool
  }

streamStatus : Decoder EventData
streamStatus =
  map2 StreamStatusStruct
    (field "streaming" bool)
    (field "recording" bool)
  |> map StreamStatus 

sceneItemVisibilityChanged : Decoder EventData
sceneItemVisibilityChanged =
  map3 SceneItemVisibilityChanged
    (field "scene-name" string)
    (field "item-name" string)
    (field "item-visible" render)
