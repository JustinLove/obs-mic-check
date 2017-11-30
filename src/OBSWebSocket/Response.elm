module OBSWebSocket.Response exposing (..)

import Json.Decode exposing (..)

type Response
  = GetVersion GetVersionStruct

type alias GetVersionStruct =
  { messageId : String
  , obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

getVersion : Decoder Response
getVersion =
  map3 GetVersionStruct
    (field "message-id" string)
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
  |> map GetVersion 
