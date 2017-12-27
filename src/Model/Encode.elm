module Model.Encode exposing (persistenceModel)

import Model exposing (PersistenceModel)
import RuleSet.Encode exposing (ruleSet)

import Json.Encode exposing (..)

persistenceModel : PersistenceModel -> Value
persistenceModel pm =
  object
    [ ("version", int 3)
    , ("ruleSet", ruleSet pm.ruleSet)
    , ("frameSampleWindow", int pm.frameSampleWindow)
    , ("frameAlarmLevel", float pm.frameAlarmLevel)
    , ("obsHost", string pm.obsHost)
    , ("obsPort", int pm.obsPort)
    ]
