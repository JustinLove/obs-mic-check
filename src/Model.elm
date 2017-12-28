module Model exposing (Model, PersistenceModel, AppMode(..), RuleKey(..), ConnectionStatus(..), makeModel)

import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..))
import Alarm exposing (Alarm(..), AlarmRepeat(..))
import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), SpecialSources, StatusReport, Challenge)

import Dict exposing (Dict)

-- MODEL

type RuleKey
  = VideoKey VideoState
  | DefaultKey

type AppMode
  = AudioRules
  | FrameRules
  | SelectVideo AudioRule
  | SelectAudio RuleKey Operator (List AudioState)

type ConnectionStatus
 = Disconnected
 | Connecting
 | Connected String
 | AuthRequired String Challenge
 | LoggingIn String
 | Authenticated String

type alias Model =
  -- application state (transient)
  { connected : ConnectionStatus
  , appMode : AppMode
  -- obs state (transient)
  , time : Int
  , currentScene : Scene
  , specialSources : SpecialSources
  , allSources : Dict String Source
  , recentStatus : List StatusReport
  -- derived state (transient)
  , activeAudioRule : AudioRule
  , audioAlarm : Alarm
  , frameAlarm : Alarm
  , alarm : Alarm
  , alarmRepeat : AlarmRepeat
  , droppedFrameRate : Float
  -- rules/config (persistent)
  , ruleSet : RuleSet
  , frameSampleWindow : Int
  , frameAlarmLevel : Float
  , obsHost : String
  , obsPort : Int
  , audioAlarmAudible : Bool
  , frameAlarmAudible : Bool
  }

type alias PersistenceModel =
  { ruleSet : RuleSet
  , frameSampleWindow : Int
  , frameAlarmLevel : Float
  , obsHost : String
  , obsPort : Int
  , audioAlarmAudible : Bool
  , frameAlarmAudible : Bool
  }

makeModel : Model
makeModel =
  { connected = Disconnected
  , appMode = AudioRules
  , time = 0
  , currentScene = { name = "-", sources = []}
  , specialSources = (SpecialSources Nothing Nothing Nothing Nothing Nothing)
  , allSources = Dict.empty
  , recentStatus = []
  , activeAudioRule = RuleSet.defaultAudio
  , audioAlarm = Silent
  , frameAlarm = Silent
  , alarm = Silent
  , alarmRepeat = Rest 0
  , droppedFrameRate = 0.0
  , ruleSet = ( RuleSet.empty RuleSet.defaultAudio )
  , frameSampleWindow = 60
  , frameAlarmLevel = 0.2
  , obsHost = "localhost"
  , obsPort = 4444
  , audioAlarmAudible = True
  , frameAlarmAudible = True
  }

