module OBSWebSocket.Response exposing (..)

import Json.Decode exposing (..)

type Response = Response String ResponseData

type ResponseData
  = GetVersion GetVersionStruct

wrapper : Decoder ResponseData -> Decoder Response
wrapper =
  map2 Response
    (field "message-id" string)

type alias GetVersionStruct =
  { obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

getVersion : Decoder Response
getVersion =
  map2 GetVersionStruct
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
  |> map GetVersion 
  |> wrapper
