import AlarmRule exposing (AlarmRule(..), VideoRule(..), AudioRule(..), checkRule)
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
  [ it "visible source, live audio" <|
    isTrue <| checkRule [ brb Visible, podcaster Live ] brbLive
  , it "visible source, muted audio" <|
    isFalse <| checkRule [ brb Visible, podcaster Muted ] brbLive
  , it "hidden source, live audio" <|
    isFalse <| checkRule [ brb Hidden, podcaster Live ] brbLive
  , it "hidden source, muted audio" <|
    isFalse <| checkRule [ brb Hidden, podcaster Muted ] brbLive
  , it "default muted audio" <|
    isTrue <| checkRule [ brb Hidden, podcaster Muted ] defaultMuted
  , it "default live audio" <|
    isFalse <| checkRule [ brb Hidden, podcaster Live ] defaultMuted
  ]

brb = layer "BRB - text 2"
podcaster = mic "Podcaster - audio"

brbLive = AlarmRule
  (SourceRule "BRB - text 2" Visible) 
  (AudioRule "Podcaster - audio" Live)

defaultMuted = AlarmRule
  DefaultRule
  (AudioRule "Podcaster - audio" Muted)

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

