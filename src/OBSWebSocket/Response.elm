module OBSWebSocket.Response exposing (..)

import OBSWebSocket.Data exposing (..)

import Json.Decode exposing (..)

type ResponseData
  = GetVersion Version
  | AuthRequired Challenge
  | AuthNotRequired
  | Authenticate
  | CurrentScene Scene
  | SceneList String (List Scene)
  | GetMuted String Audio
  | GetSpecialSources SpecialSources

response : Decoder ResponseData
response =
  (field "message-id" string)
  |> andThen (\messageId -> case messageId of
    "GetVersion" -> getVersion
    "GetAuthRequired" -> getAuthRequired
    "Authenticate" -> authenticate
    "GetCurrentScene" -> currentScene
    "GetSceneList" -> sceneList
    "GetMute" -> getMuted
    "GetSpecialSources" -> getSpecialSources
    _ -> fail "Not a known response type"
  )

getVersion : Decoder ResponseData
getVersion =
  map2 Version
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
  |> map GetVersion 

getAuthRequired : Decoder ResponseData
getAuthRequired =
  field "authRequired" bool
    |> andThen (\req -> if req then
      getChallenge |> map AuthRequired
    else
      succeed AuthNotRequired
    )

getChallenge : Decoder Challenge
getChallenge =
  map2 Challenge
    (field "challenge" string)
    (field "salt" string)

authenticate : Decoder ResponseData
authenticate =
  map2 (,)
    (field "message-id" string)
    (field "status" string)
  |> andThen (\(id, status) -> case (id, status) of
      ("Authenticate", "ok") -> succeed Authenticate
      ("Authenticate", "error") -> fail "Authentication failed"
      _ -> fail "Decoder does not apply"
    )

currentScene : Decoder ResponseData
currentScene =
  scene |> map CurrentScene

sceneList : Decoder ResponseData
sceneList =
  map2 SceneList
    (field "current-scene" string)
    (field "scenes" (list scene))

getMuted : Decoder ResponseData
getMuted =
  map2 GetMuted
    (field "name" string)
    (field "muted" muted)

getSpecialSources : Decoder ResponseData
getSpecialSources =
  map5 SpecialSources
    (maybe (field "desktop-1" string))
    (maybe (field "desktop-2" string))
    (maybe (field "mic-1" string))
    (maybe (field "mic-2" string))
    (maybe (field "mic-3" string))
  |> map GetSpecialSources
