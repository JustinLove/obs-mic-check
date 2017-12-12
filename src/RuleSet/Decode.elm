module RuleSet.Decode exposing (audioRule, audioState)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Operator(..))
import OBSWebSocket.Data exposing (Render(..), Audio(..))

import Json.Decode exposing (..)

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
