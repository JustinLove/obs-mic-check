module Model.Decode exposing (persistenceModel)

import Model exposing (PersistenceModel)
import RuleSet.Decode exposing (ruleSet)

import Json.Decode exposing (..)

persistenceModel : Decoder PersistenceModel
persistenceModel =
  map PersistenceModel
    ruleSet
