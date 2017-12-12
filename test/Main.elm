import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..), activeAudioRule, checkAudioRule)
import RuleSet.Encode
import RuleSet.Decode
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources)

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Json.Encode
import Json.Decode

main : Html msg
main =
  runAll all

all : Test
all = describe "rules"
  [ describe "multiple rules"
    [ it "visible source, live audio" <|
      isTrue <| alarmRaised [ brb Visible, podcaster Live ] basicRules
    , it "visible source, muted audio" <|
      isFalse <| alarmRaised [ brb Visible, podcaster Muted ] basicRules
    , it "hidden source, live audio" <|
      isFalse <| alarmRaised [ brb Hidden, podcaster Live ] basicRules
    , it "hidden source, muted audio" <|
      isTrue <| alarmRaised [ brb Hidden, podcaster Muted ] basicRules
    ]
  , describe "multiple layers"
    [ it "hidden sources, live audio" <|
      isFalse <| alarmRaised [ brb Hidden, starting Hidden, podcaster Live ] basicRules
    , it "brb visible, muted audio" <|
      isFalse <| alarmRaised [ brb Visible, starting Hidden, podcaster Muted ] basicRules
    , it "starting visible, muted audio" <|
      isFalse <| alarmRaised [ brb Hidden, starting Visible, podcaster Muted ] basicRules
    ]
  , describe "multiple audio"
    [ it "hidden, mics on" <|
      isFalse <| alarmRaised [ brb Hidden, podcaster Live, stepmania Live ] multiMicRules
    , it "visible, default mic on" <|
      isTrue <| alarmRaised [ brb Visible, podcaster Live, stepmania Muted ] multiMicRules
    , it "visible, stepmania mic on" <|
      isTrue <| alarmRaised [ brb Visible, podcaster Muted, stepmania Live ] multiMicRules
    , it "hidden, default mic muted" <|
      isFalse <| alarmRaised [ brb Hidden, podcaster Muted, stepmania Live ] multiMicRules
    , it "hidden, stepmania mic muted" <|
      isFalse <| alarmRaised [ brb Hidden, podcaster Live, stepmania Muted ] multiMicRules
    , it "hidden, all mics muted" <|
      isTrue <| alarmRaised [ brb Hidden, podcaster Muted, stepmania Muted ] multiMicRules
    , it "hidden, only one muted mic present" <|
      isTrue <| alarmRaised [ brb Hidden, podcaster Muted ] multiMicRules
    ]
  , describe "state encoding"
    [ it "encodes AudioState" <| eql
        """["name","Live"]"""
        (RuleSet.Encode.audioState (AudioState "name" Live) |> Json.Encode.encode 0)
    , it "encodes AudioRule" <| eql
        audioRuleJson
        (RuleSet.Encode.audioRule defaultMuted |> Json.Encode.encode 2)
    , it "encodes VideoState" <| eql
        """["name","Visible"]"""
        (RuleSet.Encode.videoState (VideoState "name" Visible) |> Json.Encode.encode 0)
    , it "encodes RuleSet" <| eql
        basicRulesJson
        (RuleSet.Encode.ruleSet basicRules |> Json.Encode.encode 2)
    , it "round trips AudioState" <| roundTrips
        RuleSet.Encode.audioState
        RuleSet.Decode.audioState
        (AudioState "name" Live)
    , it "round trips AudioRule" <| roundTrips
        RuleSet.Encode.audioRule
        RuleSet.Decode.audioRule
        defaultMuted
    , it "round trips VideoState" <| roundTrips
        RuleSet.Encode.videoState
        RuleSet.Decode.videoState
        (VideoState "name" Visible)
    ]
  ]


brb = layer "BRB - text 2"
starting = layer "Starting soon - text"
podcaster = mic "Podcaster - audio"
stepmania = mic "Podcaster - Stepmania"

basicRules = RuleSet defaultMuted [ brbLive, startingLive ]
multiMicRules = RuleSet multiMuted [ brbLive, startingLive ]

allMics audio =
  [ (AudioState "Podcaster - audio" audio)
  , (AudioState "Podcaster - Stepmania" audio)
  ]

brbLive = (,)
  (VideoState "BRB - text 2" Visible) 
  (AudioRule Any (allMics Live) 0)

startingLive = (,)
  (VideoState "Starting soon - text" Visible) 
  (AudioRule Any (allMics Live) 0)

defaultMuted =
  (AudioRule Any [AudioState "Podcaster - audio" Muted] 0)

multiMuted =
  (AudioRule All (allMics Muted) 0)

layer : String -> Render -> Source
layer name render =
  { name = name
  , render = render
  , type_ = "test"
  , volume = 1.0
  , audio = Live
  }

mic : String -> Audio -> Source
mic name audio =
  { name = name
  , render = Hidden
  , type_ = "test"
  , volume = 1.0
  , audio = audio
  }

alarmRaised sources ruleSet =
  activeAudioRule sources ruleSet
    |> (\audioRule -> checkAudioRule sources audioRule)

roundTrips : (a -> Json.Encode.Value) -> Json.Decode.Decoder a -> a -> Expectation.Expectation
roundTrips encoder decoder value =
  eql
    (Ok value)
    (value |> encoder |> Json.Decode.decodeValue decoder)

audioRuleJson = """{
  "operator": "Any",
  "audioStates": [
    [
      "Podcaster - audio",
      "Muted"
    ]
  ],
  "timeout": 0
}"""

basicRulesJson = """{
  "default": {
    "operator": "Any",
    "audioStates": [
      [
        "Podcaster - audio",
        "Muted"
      ]
    ],
    "timeout": 0
  },
  "rules": [
    [
      [
        "BRB - text 2",
        "Visible"
      ],
      {
        "operator": "Any",
        "audioStates": [
          [
            "Podcaster - audio",
            "Live"
          ],
          [
            "Podcaster - Stepmania",
            "Live"
          ]
        ],
        "timeout": 0
      }
    ],
    [
      [
        "Starting soon - text",
        "Visible"
      ],
      {
        "operator": "Any",
        "audioStates": [
          [
            "Podcaster - audio",
            "Live"
          ],
          [
            "Podcaster - Stepmania",
            "Live"
          ]
        ],
        "timeout": 0
      }
    ]
  ]
}"""
