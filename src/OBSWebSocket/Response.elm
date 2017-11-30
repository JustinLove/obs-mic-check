module OBSWebSocket.Response exposing (..)

import Json.Decode exposing (..)

type ResponseData
  = GetVersion GetVersionStruct

type alias GetVersionStruct =
  { obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

getVersion : Decoder ResponseData
getVersion =
  map2 GetVersionStruct
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
  |> map GetVersion 
