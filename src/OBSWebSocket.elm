module OBSWebSocket exposing (..)

import Crypto.Hash exposing (sha256)
import Base16
import Base64

authenticate : String -> String -> String -> String
authenticate password salt challenge =
  let
    base64Secret = swizzile (password ++ salt)
    authResponse = swizzile (base64Secret ++ challenge)
  in
    authResponse

swizzile : String -> String
swizzile s =
  s
    |> sha256
    |> String.toUpper
    |> Base16.decode
    |> Result.withDefault []
    |> Base64.encode
