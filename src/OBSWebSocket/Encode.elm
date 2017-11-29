module OBSWebSocket.Encode exposing (..)

import Json.Encode exposing (..)

type Request
  = GetVersion

getVersion : Value
getVersion =
  object
    [ ("request-type", string "GetVersion")
    , ("message-id", string "-")
    ]
