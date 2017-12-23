module Alarm exposing
  ( Alarm(..)
  , AlarmRepeat(..)
  , mergeAlarms
  , updateAlarmState
  , checkNotice
  )

type Alarm
  = Silent
  | Violation Int Int
  | Alarming Int

type AlarmRepeat
  = Rest Int
  | Notice Int

updateAlarmState : Int -> Int -> Bool -> Alarm -> Alarm
updateAlarmState time timeout violation alarm =
  if time == 0 then
    Silent
  else if not violation then
    Silent
  else
    case alarm of
      Silent -> Violation time timeout
      Violation start _ -> checkTimeout start time timeout
      Alarming start -> alarm

checkTimeout : Int -> Int -> Int -> Alarm
checkTimeout start time timeout =
  if time - start >= timeout then
    Alarming start
  else
    Violation start timeout

mergeAlarms : Alarm -> Alarm -> Alarm
mergeAlarms a b =
  if (alarmStart a) > (alarmStart b) then
    a
  else 
    b

alarmStart : Alarm -> Int
alarmStart alarm =
  case alarm of
    Silent -> 0
    Violation start _ -> start
    Alarming start -> start

checkNotice : Int -> Int -> Alarm -> Int -> AlarmRepeat -> AlarmRepeat
checkNotice on off alarm time repeat =
  case alarm of
    Silent -> Rest 0
    Violation _ _ -> Rest 0
    Alarming _->
      case repeat of
        Rest start ->
          if (time - start) >= off then
            Notice time
          else
            repeat
        Notice start ->
          if (time - start) >= on then
            Rest time
          else
            repeat

