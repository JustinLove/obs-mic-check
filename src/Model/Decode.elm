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
      _ -> v2
    )

v2 : Decoder PersistenceModel
v2 =
  map3 PersistenceModel
    (field "ruleSet" ruleSet)
    (field "frameSampleWindow" int)
    (field "frameAlarmLevel" float)

