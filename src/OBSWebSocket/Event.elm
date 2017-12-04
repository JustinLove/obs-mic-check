module OBSWebSocket.Event exposing (..)

import OBSWebSocket.Data exposing (..)

import Json.Decode exposing (..)

type EventData
  = StreamStatus StreamStatusStruct
  | SceneItemAdded String String
  | SceneItemRemoved String String
  | SceneItemVisibilityChanged String String Render

event : Decoder EventData
event =
  (field "update-type" string)
  |> andThen (\updateType -> case updateType of
    "StreamStatus" -> streamStatus
    "SceneItemAdded" -> sceneItemAdded
    "SceneItemRemoved" -> sceneItemRemoved
    "SceneItemVisibilityChanged" -> sceneItemVisibilityChanged
    _ -> fail "Not a known event update-type"
  )

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

sceneItemAdded : Decoder EventData
sceneItemAdded =
  map2 SceneItemAdded
    (field "scene-name" string)
    (field "item-name" string)

sceneItemRemoved : Decoder EventData
sceneItemRemoved =
  map2 SceneItemRemoved
    (field "scene-name" string)
    (field "item-name" string)

sceneItemVisibilityChanged : Decoder EventData
sceneItemVisibilityChanged =
  map3 SceneItemVisibilityChanged
    (field "scene-name" string)
    (field "item-name" string)
    (field "item-visible" render)
