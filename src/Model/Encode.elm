module Model.Encode exposing (persistenceModel)

import Model exposing (PersistenceModel)
import RuleSet.Encode exposing (ruleSet)

import Json.Encode exposing (..)

persistenceModel : PersistenceModel -> Value
persistenceModel pm =
  ruleSet pm.ruleSet
