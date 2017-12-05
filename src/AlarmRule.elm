module AlarmRule exposing
  ( RuleSet(..)
  , AlarmRule(..)
  , VideoState(..)
  , AudioRule(..)
  , AudioState(..)
  , Alarm(..)
  , activeRule
  , checkRule
  , matchesVideoSource
  , audioSourceNames
  , checkAudioRule
  )

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

import Set

type RuleSet
  = RuleSet AudioRule (List AlarmRule)

type AlarmRule
  = AlarmRule VideoState AudioRule

type VideoState
  = VideoState String Render

type AudioRule
  = AudioRule AudioState Int

type AudioState
  = AudioState String Audio
  | AnyAudio (List AudioState)
  | AllAudio (List AudioState)

type Alarm
  = Silent
  | Violation Int
  | Alarming Int

activeRule : List Source -> RuleSet -> AudioRule
activeRule sources (RuleSet default rules) =
  rules
    |> List.filter (checkVideo sources)
    |> List.head
    |> Maybe.map (\(AlarmRule _ audio) -> audio)
    |> Maybe.withDefault default

checkRule : List Source -> AlarmRule -> Bool
checkRule sources rule =
  (checkVideo sources rule) && (checkAudio sources rule)

matchesVideoSource : Source -> AlarmRule -> Bool
matchesVideoSource source (AlarmRule videoState _) =
  case videoState of
    VideoState sourceName _ -> 
      source.name == sourceName

audioSourceNames : RuleSet -> List String
audioSourceNames (RuleSet default rules) =
  rules
    |> List.map (\(AlarmRule _ audioRule) -> audioRule)
    |> (::) default
    |> List.map (\(AudioRule audioState _) -> audioState)
    |> List.map audioStateSourceNames
    |> List.concat
    |> Set.fromList
    |> Set.toList

audioStateSourceNames : AudioState -> List String
audioStateSourceNames state =
  case state of
    AudioState sourceName _ ->
      [ sourceName ]
    AnyAudio states ->
      states
        |> List.map audioStateSourceNames
        |> List.concat
    AllAudio states ->
      states
        |> List.map audioStateSourceNames
        |> List.concat

checkVideo : List Source -> AlarmRule -> Bool
checkVideo sources (AlarmRule videoState _) =
  case videoState of
    VideoState sourceName render -> 
      List.any (\s -> s.name == sourceName && s.render == render) sources

checkAudio : List Source -> AlarmRule -> Bool
checkAudio sources (AlarmRule _ audioRule) =
  checkAudioRule sources audioRule

checkAudioRule : List Source -> AudioRule -> Bool
checkAudioRule sources (AudioRule audioState _) =
  checkAudioState sources audioState

checkAudioState : List Source -> AudioState -> Bool
checkAudioState sources audioState =
  case audioState of
    AudioState sourceName audio ->
      audio == (sources
        |> List.filterMap (\s ->
          if s.name == sourceName then
           Just s.audio
         else
           Nothing)
        |> List.head
        |> Maybe.withDefault Muted
      )
    AnyAudio states ->
      List.any (checkAudioState sources) states
    AllAudio states ->
      List.all (checkAudioState sources) states
