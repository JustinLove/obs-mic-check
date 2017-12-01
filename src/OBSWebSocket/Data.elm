module OBSWebSocket.Data exposing (..)

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
