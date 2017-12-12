module RuleSet.Encode exposing (audioState)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..))
import OBSWebSocket.Data exposing (Render(..), Audio(..))

import Json.Encode exposing (..)

audioState : AudioState -> Value
audioState (AudioState sourceName audio) =
  list [ string sourceName, string <| toString audio ]
    
