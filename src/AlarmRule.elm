module AlarmRule exposing
  ( RuleSet(..)
  , AlarmRule(..)
  , VideoRule(..)
  , AudioRule(..)
  , alarmingRule
  , checkRule
  , matchesVideoSource
  , audioSourceNames
  )

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

import Set
import Time

type RuleSet
  = RuleSet AudioRule Time.Time (List AlarmRule)

type AlarmRule
  = AlarmRule VideoRule AudioRule Time.Time

type VideoRule
  = SourceRule String Render

type AudioRule
  = AudioRule String Audio
  | AnyAudio (List AudioRule)
  | AllAudio (List AudioRule)

alarmingRule : List Source -> RuleSet -> Maybe AudioRule
alarmingRule sources ruleSet =
  activeRule sources ruleSet
    |> (\audioRule ->
      if checkAudioRule sources audioRule then
        Just audioRule
      else
        Nothing
      )

activeRule : List Source -> RuleSet -> AudioRule
activeRule sources (RuleSet default timeout rules) =
  rules
    |> List.filter (checkVideo sources)
    |> List.head
    |> Maybe.map (\(AlarmRule _ audio _) -> audio)
    |> Maybe.withDefault default

checkRule : List Source -> AlarmRule -> Bool
checkRule sources rule =
  (checkVideo sources rule) && (checkAudio sources rule)

matchesVideoSource : Source -> AlarmRule -> Bool
matchesVideoSource source (AlarmRule videoRule _ _) =
  case videoRule of
    SourceRule sourceName _ -> 
      source.name == sourceName

audioSourceNames : RuleSet -> List String
audioSourceNames (RuleSet default timeout rules) =
  rules
    |> List.map (\(AlarmRule _ audioRule _) -> audioRule)
    |> (::) default
    |> List.map audioRuleSourceNames
    |> List.concat
    |> Set.fromList
    |> Set.toList

audioRuleSourceNames : AudioRule -> List String
audioRuleSourceNames audioRule =
  case audioRule of
    AudioRule sourceName _ ->
      [ sourceName ]
    AnyAudio rules ->
      rules
        |> List.map audioRuleSourceNames
        |> List.concat
    AllAudio rules ->
      rules
        |> List.map audioRuleSourceNames
        |> List.concat

checkVideo : List Source -> AlarmRule -> Bool
checkVideo sources (AlarmRule videoRule _ _) =
  case videoRule of
    SourceRule sourceName render -> 
      List.any (\s -> s.name == sourceName && s.render == render) sources

checkAudio : List Source -> AlarmRule -> Bool
checkAudio sources (AlarmRule _ audioRule _) =
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
