module AlarmRule exposing (AlarmRule, checkRule)

import OBSWebSocket.Response as Response exposing (Source, Render(..), Audio(..))

type alias AlarmRule =
  { videoSourceName : String
  , render : Render
  , audioSourceName : String
  , audio : Audio
  }

checkRule : List Source -> AlarmRule -> Bool
checkRule sources rule =
  let
    video = List.any (\s -> s.name == rule.videoSourceName && s.render == rule.render) sources
    audio = List.any (\s -> s.name == rule.audioSourceName && s.audio == rule.audio) sources
  in
    video && audio
