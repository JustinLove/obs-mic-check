module RuleSet.Decode exposing (audioState)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Operator(..))
import OBSWebSocket.Data exposing (Render(..), Audio(..))

import Json.Decode exposing (..)

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
