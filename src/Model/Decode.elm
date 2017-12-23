module Model.Decode exposing (persistenceModel)

import Model exposing (PersistenceModel)
import RuleSet.Decode exposing (ruleSet)

import Json.Decode exposing (..)

persistenceModel : Decoder PersistenceModel
persistenceModel =
  oneOf
    [ versioned
    , v1
    ]

v1 : Decoder PersistenceModel
v1 =
  map (\rs -> PersistenceModel rs 60 0.2)
    ruleSet

versioned : Decoder PersistenceModel
versioned =
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

