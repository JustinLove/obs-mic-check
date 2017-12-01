module OBSWebSocket.Data exposing (..)

import Json.Decode exposing (..)

type alias Version =
  { obsWebsocketVersion : String
  , obsStudioVersion : String
  -- availableRequests
  }

type alias Challenge =
  { challenge : String
  , salt : String
  }

type alias Scene =
  { name : String
  , sources : List Source
  }

type alias Source =
  { name : String
  , render : Render
  , type_ : String
  , volume : Float
  , audio : Audio
  }


type Render
  = Visible
  | Hidden

type Audio
  = Live
  | Muted

render : Decoder Render
render =
  map (\b -> if b then Visible else Hidden) bool

muted : Decoder Audio
muted =
  map (\b -> if b then Muted else Live) bool
