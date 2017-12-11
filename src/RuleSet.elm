module RuleSet exposing
  ( RuleSet(..)
  , VideoState(..)
  , AudioRule(..)
  , Operator(..)
  , AudioState(..)
  , Alarm(..)
  , activeAudioRule
  , audioSourceNames
  , checkVideoState
  , checkAudioRule
  , empty
  , insert
  , update
  , remove
  , mapDefault
  , get
  , default
  , toList
  )

import OBSWebSocket.Data exposing (Source, Render(..), Audio(..))

import Set

type RuleSet
  = RuleSet AudioRule (List AlarmRule)

type alias AlarmRule = (VideoState, AudioRule)

type VideoState
  = VideoState String Render

type AudioRule
  = AudioRule Operator (List AudioState) Int

type Operator
  = Any
  | All

type AudioState
  = AudioState String Audio

type Alarm
  = Silent
  | Violation Int
  | Alarming Int

activeAudioRule : List Source -> RuleSet -> AudioRule
activeAudioRule sources ruleSet =
  case ruleSet of
    RuleSet default rules ->
      sources
        |> List.map (\source -> VideoState source.name source.render)
        |> List.filterMap (\vs -> get vs ruleSet)
        |> List.head
        |> Maybe.withDefault default

audioSourceNames : RuleSet -> List String
audioSourceNames ruleSet =
  audioRules ruleSet
    |> List.concatMap (\(AudioRule _ audioStates _) -> audioStates)
    |> List.map (\(AudioState name _) -> name)
    |> Set.fromList
    |> Set.toList

checkVideoState : List Source -> VideoState -> Bool
checkVideoState sources (VideoState sourceName render) =
  sources
    |> List.filter (\s -> s.name == sourceName && s.render == render)
    |> List.isEmpty
    |> not

checkAudioRule : List Source -> AudioRule -> Bool
checkAudioRule sources (AudioRule operator states _) =
  case operator of
    Any ->
      states
        |> List.map (checkAudioState sources)
        |> List.foldl (||) False
    All ->
      states
        |> List.map (checkAudioState sources)
        |> List.foldl (&&) True

checkAudioState : List Source -> AudioState -> Bool
checkAudioState sources (AudioState sourceName audio) =
  audio == (sources
    |> List.filterMap (\s ->
      if s.name == sourceName then
       Just s.audio
     else
       Nothing)
    |> List.head
    |> Maybe.withDefault Muted
  )

empty : AudioRule -> RuleSet
empty default = RuleSet default []

insert : VideoState -> AudioRule -> RuleSet -> RuleSet
insert videoState audioRule (RuleSet default rules) =
  RuleSet default <|
    ( rules
    |> List.filter (\(vid, aud) -> vid /= videoState)
    |> (::) (videoState, audioRule)
    )

update : VideoState -> (Maybe AudioRule -> Maybe AudioRule) -> RuleSet -> RuleSet
update key f (RuleSet default rules) =
  RuleSet default
    <| List.filterMap (\(vid, aud) ->
      if vid == key then
        f (Just aud) |> Maybe.map (\a -> (vid, a))
      else
        (Just (vid, aud))
      ) rules

remove : VideoState -> RuleSet -> RuleSet
remove key (RuleSet default rules) =
  RuleSet default <| List.filter (\(vid, aud) -> vid /= key) rules

mapDefault : (AudioRule -> AudioRule) -> RuleSet -> RuleSet
mapDefault f (RuleSet default rules) = RuleSet (f default) rules

get : VideoState -> RuleSet -> Maybe AudioRule
get videoState (RuleSet _ rules) =
  rules
    |> List.filterMap (\(vid, aud) -> if videoState == vid then Just aud else Nothing)
    |> List.head

default : RuleSet -> AudioRule
default (RuleSet def _) = def

videoStates : RuleSet -> List VideoState
videoStates (RuleSet _ rules) =
  rules
    |> List.map Tuple.first

audioRules : RuleSet -> List AudioRule
audioRules (RuleSet default rules) =
  rules
    |> List.map Tuple.second
    |> (::) default

toList : RuleSet -> List (VideoState, AudioRule)
toList (RuleSet _ rules) = rules
