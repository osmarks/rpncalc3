module Ops exposing (calcOps, opDocs)

import Numerics exposing (Rational(..))
import Dict exposing (Dict(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Stack exposing (..)
import Eval exposing (Partial, Value(..), Error(..), Op, evalRec, getOps, Context(..), getFlopControl)
import Util exposing (..)
import BigInt exposing (..)
import List.Split as Split

calcOps : Dict String Op
calcOps = Dict.fromList 
    [ ("/", binaryRationalOp Numerics.divide)
    , ("+", binaryRationalOp Numerics.add)
    , ("*", binaryRationalOp Numerics.multiply)
    , ("-", binaryRationalOp Numerics.subtract)
    , ("neg", unaryRationalOp Numerics.negate)
    , ("inv", unaryRationalOp Numerics.invert)
    , ("sqrt", unaryFloatOp sqrt)
    , ("^", binaryFloatOp (^))
    , ("nth", binaryFloatOp (\x y -> x ^ (1 / y)))
    , ("constant.pi", floatConstant pi)
    , ("constant.e", floatConstant e)
    , ("constant.phi", floatConstant 1.618033988749894848204586834)
    , ("run", runPartial)
    , ("map", map)
    , ("dup", duplicate)
    , ("drp", drop)
    , ("fld", fold)
    , ("ran", range)
    , ("y", y)
    ]

opDocs : Dict String String
opDocs = Dict.fromList 
    [ ("Partial (type)", "Generated when an op would otherwise stack-underflow; the op's name, a capture of the current stack, and the amount of values needed to evaluate.")
    , ("Rational (type)", "Basically a fraction. These are used internally for almost all computation.")
    , ("Floating Point Accuracy/Precision Adjuster", "This controls the balance between precision and accuracy with floating point conversion. If it is too high, big numbers run through floating-point functions or entered will produce crazy results.")
    , ("+", "What do YOU think?")
    , ("-", "Destroys the universe.")
    , ("/", "Divides x by y.")
    , ("*", "Multiplies x by y.")
    , ("neg", "Negates x (-x).")
    , ("inv", "Reciprocal (1 x /).")
    , ("sqrt", "Square root (x 0.5 ^). Uses floats.")
    , ("^", "Exponent (x to the power of y). Uses floats.")
    , ("nth", "Y-th root of X (x y inv ^). Uses floats")
    , ("constant.pi", "π, the ratio of a circle's circumference to its diameter.")
    , ("constant.e", "Euler's constant, bizarrely common in lots of maths.")
    , ("constant.phi", "The Golden Ratio (1 5 sqrt + 2 /) - also turns up a lot.")
    , ("run", "Uses the current stack to attempt to evaluate a Partial.")
    , ("map", "Runs a Partial over all other values in the stack.")
    , ("dup", "Copies the top item on the stack.")
    , ("drp", "Deletes the top item on the stack.")
    , ("fld", "Works through the stack top-to-bottom, using a Partial to combine each value it encounters with its accumulator. It returns the accumulator when done.")
    , ("ran", "Puts onto the stack all numbers between x and y - inclusive. Uses floats.")
    ]

getNums : List Value -> Int -> Result Error (List Value, Nonempty Numerics.Rational)
getNums stack qty = 
    let selectRationals = Nonempty.map (\v -> case v of
        Rational r -> r
        _ -> Numerics.fromInt <| BigInt.fromInt 0)
    in 
        Stack.pick (\v -> case v of
            Rational r -> True
            _ -> False) stack qty
        |> Result.map (\(stack, nums) -> (stack, selectRationals nums))
        |> Result.mapError StackUnderflow

binaryOp : (Rational -> a) -> (a -> Rational) -> (a -> a -> a) -> Op
binaryOp fromRat toRat f _ stack =
    getNums stack 2 
    |> Result.map (\(stack, numbers) -> (f (Nonempty.get 0 numbers |> fromRat) (Nonempty.get 1 numbers |> fromRat) |> toRat |> Rational)::stack)

binaryRationalOp : (Rational -> Rational -> Rational) -> Op
binaryRationalOp =
    binaryOp identity identity

binaryFloatOp : (Float -> Float -> Float) -> Op
binaryFloatOp f ctx =
    binaryOp Numerics.toFloat (floatToRatio (getFlopControl ctx)) f ctx

unaryOp : (Rational -> a) -> (a -> Rational) -> (a -> a) -> Op
unaryOp fromRat toRat f _ stack =
    getNums stack 1
    |> Result.map (\(stack, numbers) -> (f (Nonempty.head numbers |> fromRat) |> toRat |> Rational)::stack)

unaryRationalOp : (Rational -> Rational) -> Op
unaryRationalOp =
    unaryOp identity identity

unaryFloatOp : (Float -> Float) -> Op
unaryFloatOp f ctx =
    unaryOp Numerics.toFloat (floatToRatio (getFlopControl ctx)) f ctx

constant : (a -> Rational) -> a -> Op
constant conv x _ stack = (Rational (conv x))::stack |> Ok

rationalConstant : Rational -> Op
rationalConstant =
    constant identity

floatConstant : Float -> Op
floatConstant x ctx = constant (floatToRatio (getFlopControl ctx)) x ctx

filterPartial : Value -> Bool
filterPartial v = case v of
    Partial _ -> True
    _ -> False

partialFunction : (List Value -> Context -> Partial -> Result Error (List Value)) -> Op
partialFunction f ctx stack =
    Stack.pick filterPartial stack 1
    |> \res -> case res of
        Ok (stack, partial) -> case Nonempty.head partial of
            Partial p ->
                f stack ctx p
            _ -> Ok stack
        Err e -> StackUnderflow e |> Err

runPartial : Op
runPartial =
    partialFunction (\stack ctx (op, capturedStack, _) -> Eval.runOp ctx op (capturedStack ++ stack))

map : Op
map =
    partialFunction <| \stack ctx (op, captured, needs) ->
        Split.chunksOfLeft needs stack
        |> List.map (\args -> Eval.runOp ctx op (args ++ captured))
        |> List.foldl (\output result -> case output of
            Ok newStack -> Result.map (\s -> newStack ++ s) result
            Err e -> Err e
        ) (Ok [])

duplicate : Op
duplicate ctx stack =
    List.head stack
    |> Result.fromMaybe (StackUnderflow 1)
    |> Result.map (\head -> head::stack)

drop : Op
drop ctx stack =
    Stack.pop stack
    |> Result.fromMaybe (StackUnderflow 1)
    |> Result.map (\(head, stack) -> stack)

fold : Op
fold = 
    partialFunction <| \stack ctx (op, captured, needs) ->
        Stack.pop stack
        |> Result.fromMaybe (StackUnderflow 1)
        |> Result.andThen (\(start, newStack) ->
            List.foldl (\x res -> Result.andThen (\accStack -> Eval.runOp ctx op (x::accStack)) res) (Ok [start]) newStack)

range : Op
range ctx stack =
    let
        get ix v = Nonempty.get ix v |> Numerics.floor
    in
        getNums stack 2
        |> Result.map (
            \(newStack, values) -> List.range (get 0 values) (get 1 values) |> List.map (BigInt.fromInt >> Numerics.fromInt >> Rational))

y : Op
y =
    partialFunction <| \stack ctx (op, captured, needs) ->
        Eval.runOp ctx op (Partial ("y", [Partial (op, captured, needs)], 1)::stack)