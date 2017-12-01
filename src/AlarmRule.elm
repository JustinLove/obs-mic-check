module AlarmRule exposing (AlarmRule)

import OBSWebSocket.Response as Response exposing (Render(..), Audio(..))

type alias AlarmRule =
  { videoSourceName : String
  , render : Render
  , audioSourceName : String
  , audio : Audio
  }
