module View exposing (view, ViewMsg(..), AppMode(..), RuleKey(..), ConnectionStatus(..))

import OBSWebSocket.Data exposing (Scene, Source, Render(..), Audio(..), mightBeVideoSource, mightBeAudioSource)
import RuleSet exposing (RuleSet(..), VideoState(..), AudioRule(..), Operator(..), AudioState(..), Alarm(..), checkVideoState, checkAudioRule)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Lazy exposing (lazy3)
import Json.Decode
import Dict
import Regex exposing (regex)

type ViewMsg
  = None
  | SetPassword String
  | LogOut
  | Cancel
  | SelectVideoSource String
  | SelectRuleAudioRule RuleKey
  | SelectAudioSource String
  | SelectAudioStatus String
  | SelectAudioMode Operator
  | SetTimeout RuleKey Int
  | CopyRule RuleKey
  | RemoveRule VideoState

type RuleKey
  = VideoKey VideoState
  | DefaultKey

type AppMode
  = Status
  | SelectVideo AudioRule
  | SelectAudio RuleKey Operator (List AudioState)

type ConnectionStatus
 = Disconnected
 | Connecting String
 | Connected String String
 | Authenticated String

-- VIEW

-- view : Model -> Html ViewMsg
view model =
  div
    [ classList
      [ ("alarms", alarming model.alarm)
      , ("mode-status", model.appMode == Status)
      ]
    ]
    [ displayHeader model
    , case model.appMode of
        Status -> displayStatus model
        SelectVideo _ -> displaySelectVideo model
        SelectAudio _ operator audioStates -> displaySelectAudio model operator audioStates
    ]

displayHeader model =
  header []
    [ displayConnectionStatus model.connected
    , alarmStatus model
    , if alarming model.alarm then
        audio
          [ autoplay True
          , src "167337__willy-ineedthatapp-com__pup-alert.mp3"
          ] []
      else
        text ""
    ]

displayStatus model =
  div [ id "status" ]
    [ displayRuleSet model.currentScene.sources model.ruleSet
    ]

displaySelectVideo model =
  let scene = model.currentScene in
  div [ id "select-video" ]
    [ h2 [] [ text scene.name ]
    , button [ onClick Cancel ] [ text "Cancel" ]
    , table [ class "source-list" ]
      <| List.map (displaySourceForSelect SelectVideoSource)
      <| List.filter (noCurrentRule model.ruleSet)
      <| List.filter mightBeVideoSource
      <| scene.sources
    ]

noCurrentRule : RuleSet -> Source -> Bool
noCurrentRule ruleSet source =
  Nothing == (RuleSet.get (VideoState source.name Visible) ruleSet)

displaySelectAudio model operator audioStates =
  let sources = Dict.values model.allSources in
  div [ id "select-audio" ]
    [ div []
      [ audioGroup "Any" (operator == Any)
        (SelectAudioMode Any)
      , audioGroup "All" (operator == All)
        (SelectAudioMode All)
      ]
    , table [ class "source-list" ]
      <| List.map (displayAudioSourceChoice audioStates)
      <| List.filter mightBeAudioSource
      <| sources
    ]

targetValue : Json.Decode.Decoder a -> (a -> ViewMsg) -> Json.Decode.Decoder ViewMsg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

int : Json.Decode.Decoder Int
int =
  Json.Decode.string
    |> Json.Decode.andThen (\text ->
      if validNumber text then
        Json.Decode.succeed <| getNumber text
      else
        Json.Decode.fail "not an integer"
      )

getNumber : String -> Int
getNumber s =
  String.toInt s |> Result.withDefault 0

validNumber : String -> Bool
validNumber value =
  Regex.contains (regex "^\\d+$") value

alarmStatus model =
  div [ class "alarm-status" ]
    [ label [] [ text "Alarm Status " ]
    , violated model.time model.alarm model.activeAudioRule
    ]

violated : Int -> Alarm -> AudioRule -> Html ViewMsg
violated time alarm audioRule =
  let (AudioRule _ _ timeout) = audioRule in
  case alarm of
    Silent ->
      alarmTime timeout 0
    Violation start ->
      alarmTime timeout (time - start)
    Alarming start ->
      alarmTime timeout (time - start)

alarmTime : Int -> Int -> Html ViewMsg
alarmTime max val =
  span []
    [ progress
      [ Html.Attributes.max <| toString max
      , value <| toString val
      ] []
    , text " "
    , if val > 0 then text <| toString val else text ""
    ]

displayConnectionStatus : ConnectionStatus -> Html ViewMsg
displayConnectionStatus connected =
  div [ class "connection-status" ]
    [ case connected of
      Disconnected ->
        input
          [ type_ "password"
          , on "change" <| targetValue Json.Decode.string SetPassword
          , placeholder "OBS Websockets password"
          ] []
      Connecting _ ->
        div [ class "connecting" ] [ text "Connecting" ]
      Connected _ version->
        div [ class "connected" ]
          [ text ("Connected (not authenticated) OBS v" ++ version)
          , text " "
          , button [ id "logout", onClick LogOut ] [ text "log out" ]
          ]
      Authenticated version->
        div [ class "authenticated" ]
          [ text ("Authenticated OBS v" ++ version)
          , text " "
          , button [ id "logout", onClick LogOut ] [ text "log out" ]
          ]
   ]

displayRuleSet : List Source -> RuleSet -> Html ViewMsg
displayRuleSet sources ruleSet =
  let
    activeVideoState = RuleSet.activeVideoState sources ruleSet
    sourceOrder = sources
      |> List.indexedMap (\index source -> (source.name, index))
      |> Dict.fromList
    ruleSourceOrder = ruleSet
      |> RuleSet.toList
      |> List.map (\((VideoState name _), _) -> name)
      |> List.sort
      |> List.indexedMap (\index name -> (name, index + 1000))
      |> Dict.fromList
    copyable = not <| List.isEmpty sources
  in
  div []
    [ ruleSet
      |> RuleSet.toList
      |> List.sortBy (\((VideoState name _), _) ->
        Dict.get name sourceOrder
          |> Maybe.withDefault (Dict.get name ruleSourceOrder
            |> Maybe.withDefault 2000)
      )
      |> List.map (\rule ->
          let
            videoState = Tuple.first rule
            (VideoState name _) = videoState
          in
          lazy3 displayRule
          copyable
          (ruleClasses 
            ((Just videoState) == activeVideoState)
            (checkRule sources rule)
            (not <| Dict.member name sourceOrder)
          )
          rule
        )
      |> (flip List.append)
        [ lazy3 displayDefaultRule
          copyable
          (ruleClasses
            (Nothing == activeVideoState)
            ((Nothing == activeVideoState) && (checkAudioRule sources (RuleSet.default ruleSet)))
            False
          )
          (RuleSet.default ruleSet) ]
      |> List.append
        [ th [ class "delete" ] [ text "Del" ]
        , th [] [ text "Video Source" ]
        , th [] [ text "Audio Status" ]
        , th [] [ text "Seconds" ]
        , th [] [ text "Copy" ]
        ]
      |> table [ class "rules" ]
    ]

checkRule : List Source -> (VideoState, AudioRule) -> Bool
checkRule sources (video, audio) =
  (checkVideoState sources video) && (checkAudioRule sources audio)

alarming : Alarm -> Bool
alarming alarm =
  case alarm of
    Silent -> False
    Violation _ -> False
    Alarming _ -> True

displaySourceForSelect : (String -> ViewMsg) -> Source -> Html ViewMsg
displaySourceForSelect tagger source =
  tr
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    , onClick (tagger source.name)
    ]
    [ td [ class "icon" ] [ renderStatus source.render ]
    , td [ class "icon" ] [ audioStatus source.audio ]
    , td [] [ text source.name ]
    , td [] [ em [] [ text source.type_ ] ]
    ]

displayAudioSourceChoice : List AudioState -> Source -> Html ViewMsg
displayAudioSourceChoice audioStates source =
  let status = audioStates
    |> List.filterMap (matchingAudioStatus source.name)
    |> List.head
  in
  tr
    [ classList 
      [ ("hidden", source.render == Hidden)
      , ("visible", source.render == Visible)
      , ("source", True)
      ]
    ]
    [ td [ class "icon" ]
      [ input
        [ type_ "checkbox"
        , Html.Attributes.name (source.name ++ "-selected")
        , id (source.name ++ "-selected")
        , value "selected"
        , onCheck (\_ -> (SelectAudioSource source.name))
        , checked (status /= Nothing)
        ] []
      ]
    , td [ class "icon" ] [ renderStatus source.render ]
    , td [ class "icon", onClick (SelectAudioStatus source.name) ]
      [ status |> Maybe.map audioStatus |> Maybe.withDefault (text "")]
    , td [] [ text source.name ]
    , td [] [ em [] [ text source.type_ ] ]
    ]

matchingAudioStatus : String -> AudioState -> Maybe Audio
matchingAudioStatus sourceName (AudioState name status) =
  if name == sourceName then Just status else Nothing

renderStatus : Render -> Html ViewMsg
renderStatus render =
  case render of
    Visible -> span [ class "video" ] [ text "O" ]
    Hidden -> span [ class "video" ] [ text "-" ]

audioStatus : Audio -> Html ViewMsg
audioStatus audio =
  case audio of
    Muted -> span [ class "audio muted" ] [ text "<X " ]
    Live -> span [ class "audio live" ] [ text "<))" ]

displayRule : Bool -> Attribute ViewMsg -> (VideoState, AudioRule) -> Html ViewMsg
displayRule copyable classes (video, audio) =
  tr [ classes ]
    <| List.append
      [ td [ class "delete" ] [ button [ onClick (RemoveRule video) ] [ text "X" ] ]
      , (displayVideoRule video)
      ]
      (displayAudioRule copyable (VideoKey video) audio)

displayDefaultRule : Bool -> Attribute ViewMsg -> AudioRule -> Html ViewMsg
displayDefaultRule copyable classes audioRule =
  tr [ classes ]
    <| List.append
      [ td [ class "delete" ] []
      , td [] [ text "default " ]
      ]
      (displayAudioRule copyable DefaultKey audioRule)

ruleClasses : Bool -> Bool -> Bool -> Attribute ViewMsg
ruleClasses active violation missing =
  classList
    [ ("active", active)
    , ("violation", violation)
    , ("missing", missing)
    ]

displayVideoRule : VideoState -> Html ViewMsg
displayVideoRule videoState =
  case videoState of 
    VideoState sourceName render ->
      td []
        [ renderStatus render
        , text " "
        , text sourceName
        ]

displayAudioRule : Bool -> RuleKey -> AudioRule -> List (Html ViewMsg)
displayAudioRule copyable key (AudioRule operator states timeout) =
  [ td
    []
    [ button
      [ onClick <| SelectRuleAudioRule key ]
      [ text <| toString operator
      , ul []
        <| List.map (\e -> li [] [e])
        <| List.map displayAudioState states
      ]
    ]
  , td []
    [ input
      [ value <| toString timeout
      , type_ "number"
      , Html.Attributes.min "0"
      , on "change" <| targetValue int (SetTimeout key)
      , class "timeout"
      ] []
    ]
  , td []
    [ button
      [ onClick (CopyRule key), disabled (not copyable) ]
      [ text "Copy" ]
    ]
  ]

displayAudioState : AudioState -> Html ViewMsg
displayAudioState (AudioState sourceName audio) =
  div []
    [ text sourceName
    , text " "
    , audioStatus audio
    ]

audioGroup : String -> Bool -> ViewMsg -> Html ViewMsg
audioGroup name isSelected msg =
  input
    [ type_ "button"
    , Html.Attributes.name ("audio-group-" ++ name)
    , id ("audio-group-" ++ name)
    , value name
    , onClick msg
    , classList
      [ ("current-mode", isSelected)
      , ("audio-mode", True)
      ]
    ] []
