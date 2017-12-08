module OBSWebSocket.Request exposing (..)

import Json.Encode exposing (..)

type Request
  = GetVersion
  | GetAuthRequired
  | Authenticate String
  | GetCurrentScene
  | GetSceneList
  | GetMute String
  | GetSpecialSources

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

getCurrentScene : Value
getCurrentScene = basicRequest GetCurrentScene

getSceneList : Value
getSceneList = basicRequest GetSceneList

getMute : String -> Value
getMute source =
  object
    [ ("request-type", string "GetMute")
    , ("message-id", string "GetMute")
    , ("source", string source)
    ]

getSpecialSources : Value
getSpecialSources = basicRequest GetSpecialSources
