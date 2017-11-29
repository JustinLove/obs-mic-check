module OBSWebSocket.Decode exposing (..)

import Json.Decode exposing (..)

type alias GetVersion =
  { messageId : String
  , obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

getVersion : Decoder GetVersion
getVersion =
  map3 GetVersion
    (field "message-id" string)
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
