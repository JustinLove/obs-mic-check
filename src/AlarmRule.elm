module AlarmRule exposing (AlarmRule(..), VideoRule(..), AudioRule(..), alarmingRule, checkRule, matchesVideoSource)

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

type AlarmRule
  = AlarmRule VideoRule AudioRule

type VideoRule
  = SourceRule String Render
  | DefaultRule

type AudioRule
  = AudioRule String Audio

alarmingRule : List Source -> List AlarmRule -> Maybe AlarmRule
alarmingRule sources rules =
  activeRule sources rules
    |> Maybe.andThen (\rule ->
      if checkAudio sources rule then
        Just rule
      else
        Nothing
      )

activeRule : List Source -> List AlarmRule -> Maybe AlarmRule
activeRule sources rules =
  List.head <| List.filter (checkVideo sources) rules

checkRule : List Source -> AlarmRule -> Bool
checkRule sources rule =
  (checkVideo sources rule) && (checkAudio sources rule)

matchesVideoSource : Source -> AlarmRule -> Bool
matchesVideoSource source (AlarmRule videoRule _) =
  case videoRule of
    SourceRule sourceName _ -> 
      source.name == sourceName
    DefaultRule ->
      False

checkVideo : List Source -> AlarmRule -> Bool
checkVideo sources (AlarmRule videoRule _) =
  case videoRule of
    SourceRule sourceName render -> 
      List.any (\s -> s.name == sourceName && s.render == render) sources
    DefaultRule ->
      True

checkAudio : List Source -> AlarmRule -> Bool
checkAudio sources (AlarmRule _ audioRule) =
  case audioRule of
    AudioRule sourceName audio -> 
      List.any (\s -> s.name == sourceName && s.audio == audio) sources
