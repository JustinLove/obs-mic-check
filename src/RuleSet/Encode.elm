module RuleSet.Encode exposing (videoState, audioRule, audioState)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), AudioState(..), Operator(..))
import OBSWebSocket.Data exposing (Render(..), Audio(..))

import Json.Encode exposing (..)

videoState : VideoState -> Value
videoState (VideoState sourceName render) =
  list [ string sourceName, string <| toString render ]

audioRule : AudioRule -> Value
audioRule (AudioRule operator audioStates timeout) =
  object
    [ ("operator", string <| toString operator)
    , ("audioStates", list <| List.map audioState audioStates)
    , ("timeout", int timeout)
    ]

audioState : AudioState -> Value
audioState (AudioState sourceName audio) =
  list [ string sourceName, string <| toString audio ]
