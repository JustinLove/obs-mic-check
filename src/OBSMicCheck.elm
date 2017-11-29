module OBSMicCheck exposing (..)

import View exposing (view, ViewMsg(..))

import Html

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  {
  }

init : (Model, Cmd Msg)
init =
  ({}, Cmd.none)

-- UPDATE

type Msg
  = View ViewMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    View None -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
