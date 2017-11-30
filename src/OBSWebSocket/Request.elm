module OBSWebSocket.Request exposing (..)

import Json.Encode exposing (..)

type Request
  = GetVersion
  | GetAuthRequired

basicRequest : Request -> Value
basicRequest req =
  object
    [ ("request-type", string <| toString req)
    , ("message-id", string "-")
    ]

getVersion : Value
getVersion = basicRequest GetVersion

getAuthRequired : Value
getAuthRequired = basicRequest GetAuthRequired
