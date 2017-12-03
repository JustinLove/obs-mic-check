module AlarmRule exposing (AlarmRule(..), VideoRule(..), AudioRule(..), checkRule, matchesVideoSource)

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

type AlarmRule
  = AlarmRule VideoRule AudioRule

type VideoRule
  = SourceRule String Render
  | DefaultRule

type AudioRule
  = AudioRule String Audio

checkRule : List Source -> AlarmRule -> Bool
checkRule sources (AlarmRule video audio) =
  (checkVideo sources video) && (checkAudio sources audio)

matchesVideoSource : Source -> AlarmRule -> Bool
matchesVideoSource source (AlarmRule videoRule _) =
  case videoRule of
    SourceRule sourceName _ -> 
      source.name == sourceName
    DefaultRule ->
      False

checkVideo : List Source -> VideoRule -> Bool
checkVideo sources videoRule =
  case videoRule of
    SourceRule sourceName render -> 
      List.any (\s -> s.name == sourceName && s.render == render) sources
    DefaultRule ->
      True

checkAudio : List Source -> AudioRule -> Bool
checkAudio sources audioRule =
  case audioRule of
    AudioRule sourceName audio -> 
      List.any (\s -> s.name == sourceName && s.audio == audio) sources
