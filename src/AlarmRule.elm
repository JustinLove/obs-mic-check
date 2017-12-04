module AlarmRule exposing (AlarmRule(..), VideoRule(..), AudioRule(..), alarmingRule, checkRule, matchesVideoSource, audioSourceNames)

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

type AlarmRule
  = AlarmRule VideoRule AudioRule

type VideoRule
  = SourceRule String Render
  | DefaultRule

type AudioRule
  = AudioRule String Audio
  | AnyAudio (List AudioRule)
  | AllAudio (List AudioRule)

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

audioSourceNames : AudioRule -> List String
audioSourceNames audioRule =
  case audioRule of
    AudioRule sourceName _ ->
      [ sourceName ]
    AnyAudio rules ->
      rules
        |> List.map audioSourceNames
        |> List.concat
    AllAudio rules ->
      rules
        |> List.map audioSourceNames
        |> List.concat

checkVideo : List Source -> AlarmRule -> Bool
checkVideo sources (AlarmRule videoRule _) =
  case videoRule of
    SourceRule sourceName render -> 
      List.any (\s -> s.name == sourceName && s.render == render) sources
    DefaultRule ->
      True

checkAudio : List Source -> AlarmRule -> Bool
checkAudio sources (AlarmRule _ audioRule) =
  checkAudioRule sources audioRule

checkAudioRule : List Source -> AudioRule -> Bool
checkAudioRule sources audioRule =
  case audioRule of
    AudioRule sourceName audio ->
      audio == (sources
        |> List.filterMap (\s ->
          if s.name == sourceName then
           Just s.audio
         else
           Nothing)
        |> List.head
        |> Maybe.withDefault Muted
      )
    AnyAudio rules ->
      List.any (checkAudioRule sources) rules
    AllAudio rules ->
      List.all (checkAudioRule sources) rules
