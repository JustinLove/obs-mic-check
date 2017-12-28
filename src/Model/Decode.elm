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
      2 -> fail "previous version"
      3 -> fail "previous version"
      4 -> v4
      _ -> v4
    )

v4 : Decoder PersistenceModel
v4 =
  map7 PersistenceModel
    (field "ruleSet" ruleSet)
    (field "frameSampleWindow" int)
    (field "frameAlarmLevel" float)
    (field "obsHost" string)
    (field "obsPort" int)
    (field "audioAlarmAudible" bool)
    (field "frameAlarmAudible" bool)

