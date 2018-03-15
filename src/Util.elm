module Util exposing (..)

import Numerics exposing (Rational(..))
import BigInt exposing (BigInt(..))

floatToRatio : Int -> Float -> Rational
floatToRatio accuracy f =
    let
        acc = BigInt.fromInt accuracy
    in
        Numerics.over (f * Numerics.bigintToFloat acc |> floor |> BigInt.fromInt) acc