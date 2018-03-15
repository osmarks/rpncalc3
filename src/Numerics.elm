module Numerics
    exposing
        ( gcd
        , add
        , subtract
        , multiply
        , multiplyByInt
        , divide
        , divideByInt
        , divideIntBy
        , negate
        , invert
        , Rational
        , over
        , denominator
        , numerator
        , split
        , toFloat
        , fromInt
        , eq
        , ne
        , gt
        , lt
        , ge
        , le
        , compare
        , max
        , min
        , isZero
        , isInfinite
        , round
        , floor
        , ceiling
        , truncate
        , bigintToFloat
        )

{-| A simple module providing a ratio type for rational numbers

# Types
@docs Rational

# Construction
@docs over, fromInt

# Comparison
@docs eq, ne, gt, lt, ge, le, max, min, compare

# Mathematics
@docs add, subtract, multiply, multiplyByInt
@docs divide, divideByInt, divideIntBy, negate
@docs isZero, isInfinite, round, floor, ceiling, truncate

# Elimination
@docs numerator, denominator, split

# Utils
@docs gcd, invert, toFloat

-}

import Basics exposing (..)
import BigInt exposing (..)

{-| "Arbitrary" (up to `max_int` size) precision fractional numbers. Think of
    it as the length of a rigid bar that you've constructed from a bunch of
    initial bars of the same fixed length
    by the operations of gluing bars together and shrinking a
    given bar so that an integer number of copies of it glues together to
    make another given bar.

-}
type Rational
    = Rational BigInt BigInt


{-| The biggest number that divides both arguments (the greatest common divisor).
-}
gcd : BigInt -> BigInt -> BigInt
gcd a b =
    if b == BigInt.fromInt 0 then
        a
    else
        gcd b (BigInt.mod a b)

{- Normalisation of rationals with negative denominators

   Rational 1 (-3) becomes Rational (-1) 3

   Rational (-1) (-3) becomes Rational 1 3
-}


normalize (Rational p q) =
    let
        k = BigInt.mul
            (gcd p q)
                  (if BigInt.lt q (BigInt.fromInt 0) then
                    BigInt.fromInt -1
                   else
                    BigInt.fromInt 1
                  )
    in
        Rational (BigInt.div p k) (BigInt.div q k)



{- Add or subtract two rationals :-
   f can be (+) or (-)
-}


addsub : (BigInt -> BigInt -> BigInt) -> Rational -> Rational -> Rational
addsub f (Rational a b) (Rational c d) =
    normalize (Rational (f (BigInt.mul a d) (BigInt.mul b c)) (BigInt.mul b d))


{-| Addition. It's like gluing together two bars of the given lengths.
-}
add : Rational -> Rational -> Rational
add =
    addsub BigInt.add


{-| subtraction. Is it like ungluing two bars of the given lengths?
-}
subtract : Rational -> Rational -> Rational
subtract =
    addsub BigInt.sub


{-| Mulitplication. `mulitply x (c / d)` is the length of the bar that you'd get
    if you glued `c` copies of a bar of length `x` end-to-end and then shrunk it
    down enough so that `d` copies of the shrunken bar would fit in the big
    glued bar.
-}
multiply : Rational -> Rational -> Rational
multiply (Rational a b) (Rational c d) =
    normalize (Rational (BigInt.mul a c) (BigInt.mul b d))


{-| Multiply a rational by an BigInt
-}
multiplyByInt : Rational -> BigInt -> Rational
multiplyByInt (Rational a b) i =
    normalize (Rational (BigInt.mul a i) b)


{-| Divide two rationals
-}
divide : Rational -> Rational -> Rational
divide r (Rational c d) =
    multiply r (Rational d c)


{-| Divide a rational by an BigInt
-}
divideByInt : Rational -> BigInt -> Rational
divideByInt r i =
    normalize (divide r (fromInt i))


{-| Divide an BigInt by a rational
-}
divideIntBy : BigInt -> Rational -> Rational
divideIntBy i r =
    normalize (divide (fromInt i) r)



{- This implementation gives the wrong precedence
   divideByInt r i =
     normalize (multiplyByInt (invert r) i)
-}


{-| multiplication by `-1`.
-}
negate : Rational -> Rational
negate (Rational a b) =
    Rational (BigInt.negate a) b


{-| invert the rational. r becomes 1/r.
-}
invert : Rational -> Rational
invert (Rational a b) =
    normalize (Rational b a)


{-| `over x y` is like `x / y`.
-}
over : BigInt -> BigInt -> Rational
over x y =
    if (BigInt.lt y <| BigInt.fromInt 0) then
        normalize (Rational (BigInt.negate x) (BigInt.negate y))
    else
        normalize (Rational x y)


{-| `fromInt x = over x 1`
-}
fromInt : BigInt -> Rational
fromInt x =
    over x (BigInt.fromInt 1)


{-| -}
numerator : Rational -> BigInt
numerator (Rational a _) =
    a


{-| -}
denominator : Rational -> BigInt
denominator (Rational _ b) =
    b


{-| `split x = (numerator x, denominator x)`
-}
split : Rational -> ( BigInt, BigInt )
split (Rational a b) =
    ( a, b )

bigintToFloat : BigInt -> Float
bigintToFloat b =
    let res = BigInt.toString b |> String.toInt
    in case res of
        Ok x -> Basics.toFloat x
        Err e -> 0

{-| -}
toFloat : Rational -> Float
toFloat (Rational a b) =
    bigintToFloat a / bigintToFloat b


{-| -}
eq : Rational -> Rational -> Bool
eq a b =
    rel (==) a b


{-| -}
ne : Rational -> Rational -> Bool
ne a b =
    rel (/=) a b


{-| -}
gt : Rational -> Rational -> Bool
gt a b =
    rel BigInt.gt a b


{-| -}
lt : Rational -> Rational -> Bool
lt a b =
    rel BigInt.lt a b


{-| -}
ge : Rational -> Rational -> Bool
ge a b =
    rel BigInt.gte a b


{-| -}
le : Rational -> Rational -> Bool
le a b =
    rel BigInt.lte a b


{-| -}
compare : Rational -> Rational -> Order
compare a b =
    Basics.compare (toFloat a) (toFloat b)


{-| -}
max : Rational -> Rational -> Rational
max a b =
    if gt a b then
        a
    else
        b


{-| -}
min : Rational -> Rational -> Rational
min a b =
    if lt a b then
        a
    else
        b


{-| -}
isZero : Rational -> Bool
isZero r =
    (BigInt.fromInt 0) == (numerator r)


{-| -}
isInfinite : Rational -> Bool
isInfinite r =
    (BigInt.fromInt 0) == (denominator r)


{-| -}
round : Rational -> Int
round =
    toFloat >> Basics.round


{-| -}
floor : Rational -> Int
floor =
    toFloat >> Basics.floor


{-| -}
ceiling : Rational -> Int
ceiling =
    toFloat >> Basics.ceiling


{-| -}
truncate : Rational -> Int
truncate =
    toFloat >> Basics.truncate


rel : (BigInt -> BigInt -> Bool) -> Rational -> Rational -> Bool
rel relop a b =
    relop (BigInt.mul (numerator a) (denominator b)) (BigInt.mul (numerator b) (denominator a))
