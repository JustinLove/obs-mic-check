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

type alias SpecialSources =
  { desktop1 : Maybe String
  , desktop2 : Maybe String
  , mic1 : Maybe String
  , mic2 : Maybe String
  , mic3 : Maybe String
  }

scene : Decoder Scene
scene =
  map2 Scene
    (oneOf
      [ (field "name" string)
      , (field "scene-name" string)
      ]
    )
    (field "sources" (list source))

source : Decoder Source
source =
  map5 Source
    (field "name" string)
    (field "render" render)
    (field "type" string)
    (field "volume" float)
    (succeed Live)

render : Decoder Render
render =
  map (\b -> if b then Visible else Hidden) bool

muted : Decoder Audio
muted =
  map (\b -> if b then Muted else Live) bool
