module Eval exposing (..)

import Expr exposing (..)
import Ratio exposing (Rational(..))
import Dict exposing (Dict(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Stack exposing (..)

type Error = MathematicalImpossibility | StackUnderflow Int | OpNotFound OpName

type alias Partial = (OpName, List Value, Int)

type Value =
    Rational Rational | Partial Partial

evalGroup : List Expr -> Result Error (List Value)
evalGroup g =
    List.foldl (\expr result -> Result.andThen (\stack -> evalRec expr stack) result) (Ok []) g

-- If anything but a StackUnderflow results, just let it go through.
-- If StackUnderflow occurs, convert it to Partial and push the Partial to stack.
handleOpResult : OpName -> List Value -> Result Error (List Value) -> Result Error (List Value)
handleOpResult opname stack r =
    case r of
        Ok x -> r
        Err e ->
            case e of
                StackUnderflow missing ->
                    [Partial (opname, stack, missing)]
                    |> Ok
                _ -> r

evalRec : Expr -> List Value -> Result Error (List Value)
evalRec e s =
    case e of
        Num x ->
            Ratio.fromInt x
            |> Rational
            |> \v -> v::s
            |> Ok
        Group g ->
            evalGroup g
            |> Result.map (\groupStack -> groupStack ++ s)
        Op o ->
            case Dict.get o ops of
                Just opF ->
                    opF s
                    |> handleOpResult o s
                Nothing -> OpNotFound o |> Err

eval : Expr -> Result Error (List Value)
eval e =
    evalRec e []

type alias Op = List Value -> Result Error (List Value)

ops : Dict OpName Op
ops = Dict.fromList [
    ("/", binaryOp Ratio.divide),
    ("+", binaryOp Ratio.add),
    ("*", binaryOp Ratio.multiply),
    ("-", binaryOp Ratio.subtract)
    ]

getNums : List Value -> Int -> Result Int (List Value, Nonempty Ratio.Rational)
getNums stack qty = 
    let selectRationals = Nonempty.map (\v -> case v of
        Rational r -> r
        _ -> Ratio.fromInt 0)
    in 
        Stack.pick (\v -> case v of
            Rational r -> True
            _ -> False) stack qty
        |> Result.map (\(stack, nums) -> (stack, selectRationals nums))

binaryOp : (Rational -> Rational -> Rational) -> Op
binaryOp f stack =
    getNums stack 2 
    |> Result.mapError StackUnderflow
    |> Debug.log "BINOP"
    |> Result.map (\(stack, numbers) -> (f (Nonempty.get 0 numbers) (Nonempty.get 1 numbers) |> Rational)::stack)

prettyPrintValue : Value -> String
prettyPrintValue v = case v of
    Rational r -> Ratio.toFloat r |> toString
    Partial (op, stack, missing) -> String.join " " <| ["Partial:", op, "["] ++ List.map prettyPrintValue stack ++ ["]", toString missing]

prettyPrintError : Error -> String
prettyPrintError e = case e of
    MathematicalImpossibility -> "Does not compute"
    OpNotFound n -> String.concat ["Operator \"", n, "\" not found"]
    StackUnderflow n -> String.join " " ["Stack underflowed by", toString n, "items"]