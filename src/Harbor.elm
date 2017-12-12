port module Harbor exposing (..)

port save : String -> Cmd msg
port loaded : (String -> msg) -> Sub msg
