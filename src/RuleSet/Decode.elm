module RuleSet.Decode exposing (ruleSet, videoState, audioRule, audioState)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Operator(..))
import OBSWebSocket.Data exposing (Render(..), Audio(..))

import Json.Decode exposing (..)

ruleSet : Decoder RuleSet
ruleSet =
  map2 RuleSet
    (field "default" audioRule)
    (field "rules" <| list alarmRule)

alarmRule : Decoder (VideoState, AudioRule)
alarmRule =
  map2 (,)
    (index 0 videoState)
    (index 1 audioRule)

videoState : Decoder VideoState
videoState =
  map2 VideoState
    (index 0 string)
    (index 1 render)

render : Decoder Render
render =
  string |> andThen (\s -> case s of
    "Visible" -> succeed Visible
    "Hidden" -> succeed Hidden
    _ -> fail "Unknown render status"
  )

audioRule : Decoder AudioRule
audioRule =
  map3 AudioRule
    (field "operator" operator)
    (field "audioStates" <| list audioState)
    (field "timeout" int)

operator : Decoder Operator
operator =
  string |> andThen (\s -> case s of
    "Any" -> succeed Any
    "All" -> succeed All
    _ -> fail "Unknown AudioRule operator"
  )

audioState : Decoder AudioState
audioState =
  map2 AudioState
    (index 0 string)
    (index 1 audio)

audio : Decoder Audio
audio =
  string |> andThen (\s -> case s of
    "Live" -> succeed Live
    "Muted" -> succeed Muted
    _ -> fail "Unknown audio status value"
  )
