module Model.Decode exposing (persistenceModel)

import Model exposing (PersistenceModel)
import RuleSet.Decode exposing (ruleSet)

import Json.Decode exposing (..)

persistenceModel : Decoder PersistenceModel
persistenceModel =
  field "version" int
  |> andThen (\version ->
    case version of
      1 -> fail "previous version"
      2 -> v2
      3 -> v3
      _ -> v3
    )

v2 : Decoder PersistenceModel
v2 =
  map5 PersistenceModel
    (field "ruleSet" ruleSet)
    (field "frameSampleWindow" int)
    (field "frameAlarmLevel" float)
    (succeed "localhost")
    (succeed 4444)

v3 : Decoder PersistenceModel
v3 =
  map5 PersistenceModel
    (field "ruleSet" ruleSet)
    (field "frameSampleWindow" int)
    (field "frameAlarmLevel" float)
    (field "obsHost" string)
    (field "obsPort" int)

