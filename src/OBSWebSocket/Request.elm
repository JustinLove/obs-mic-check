module OBSWebSocket.Request exposing (..)

import Json.Encode exposing (..)

type Request
  = GetVersion
  | GetAuthRequired
  | Authenticate String

basicRequest : Request -> Value
basicRequest req =
  object
    [ ("request-type", string <| toString req)
    , ("message-id", string <| toString req)
    ]

getVersion : Value
getVersion = basicRequest GetVersion

getAuthRequired : Value
getAuthRequired = basicRequest GetAuthRequired

authenticate : String -> Value
authenticate auth =
  object
    [ ("request-type", string "Authenticate")
    , ("message-id", string "Authenticate")
    , ("auth", string auth)
    ]
