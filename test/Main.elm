import AlarmRule exposing (RuleSet(..), AlarmRule(..), VideoState(..), AudioRule(..), AudioState(..), activeRule, checkRule, checkAudioRule)
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources)

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)

main : Html msg
main =
  runAll all

all : Test
all = describe "rules"
  [ describe "single rules"
    [ it "visible source, live audio" <|
      isTrue <| checkRule [ brb Visible, podcaster Live ] brbLive
    , it "visible source, muted audio" <|
      isFalse <| checkRule [ brb Visible, podcaster Muted ] brbLive
    , it "hidden source, live audio" <|
      isFalse <| checkRule [ brb Hidden, podcaster Live ] brbLive
    , it "hidden source, muted audio" <|
      isFalse <| checkRule [ brb Hidden, podcaster Muted ] brbLive
    ]
  , describe "multiple rules"
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

brbLive = AlarmRule
  (VideoState "BRB - text 2" Visible) 
  (AudioRule (AnyAudio (allMics Live)) 0)

startingLive = AlarmRule
  (VideoState "Starting soon - text" Visible) 
  (AudioRule (AnyAudio (allMics Live)) 0)

defaultMuted =
  (AudioRule (AudioState "Podcaster - audio" Muted) 0)

multiMuted =
  (AudioRule (AllAudio (allMics Muted)) 0)

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
  activeRule sources ruleSet
    |> (\audioRule -> checkAudioRule sources audioRule)
