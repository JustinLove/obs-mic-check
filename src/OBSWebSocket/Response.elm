module OBSWebSocket.Response exposing (..)

import Json.Decode exposing (..)

type ResponseData
  = GetVersion Version
  | AuthRequired Challenge
  | AuthNotRequired
  | Authenticate
  | CurrentScene Scene

response : Decoder ResponseData
response =
  oneOf
    [ getVersion
    , getAuthRequired
    , authenticate
    , currentScene
    ]

type alias Version =
  { obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

getVersion : Decoder ResponseData
getVersion =
  map2 Version
    (field "obs-websocket-version" string)
    (field "obs-studio-version" string)
  |> map GetVersion 

type alias Challenge =
  { challenge : String
  , salt : String
  }

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

type alias Scene =
  { name : String
  , sources : List Source
  }

type alias Source =
  { name : String
  , render : Bool
  , type_ : String
  , volume : Float
  }

currentScene : Decoder ResponseData
currentScene =
  scene |> map CurrentScene

scene : Decoder Scene
scene =
  map2 Scene
    (field "name" string)
    (field "sources" (list source))

source : Decoder Source
source =
  map4 Source
    (field "name" string)
    (field "render" bool)
    (field "type" string)
    (field "volume" float)
