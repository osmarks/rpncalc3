module Eval exposing (..)

import Expr exposing (..)
import Numerics exposing (Rational(..))
import Dict exposing (Dict(..))
import Util exposing (..)

type Error = MathematicalImpossibility | StackUnderflow Int | OpNotFound String | ParseError String

type alias Partial = (String, List Value, Int)

type Value =
    Rational Rational | Partial Partial

type Context = Context (Dict String Op) Int
type alias Op = Context -> List Value -> Result Error (List Value)

getOps : Context -> Dict String Op
getOps (Context o _) = o

getFlopControl : Context -> Int
getFlopControl (Context _ f) = f

evalGroup : Context -> List Expr -> Result Error (List Value)
evalGroup ctx g =
    List.foldl (\expr result -> Result.andThen (\stack -> evalRec ctx expr stack) result) (Ok []) g

-- If anything but a StackUnderflow results, just let it go through.
-- If StackUnderflow occurs, convert it to Partial and push the Partial to stack.
handleOpResult : String -> List Value -> Result Error (List Value) -> Result Error (List Value)
handleOpResult opname stack r =
    case r of
        Ok x -> r
        Err e ->
            case e of
                StackUnderflow missing ->
                    [Partial (opname, stack, missing)]
                    |> Ok
                _ -> r

runOp : Context -> String -> List Value -> Result Error (List Value)
runOp ctx name stack =
    case Dict.get name (getOps ctx) of
        Just opF ->
            opF ctx stack
            |> handleOpResult name stack
        Nothing -> OpNotFound name |> Err

evalRec : Context -> Expr -> List Value -> Result Error (List Value)
evalRec ctx e s =
    case e of
        Float x ->
            floatToRatio (getFlopControl ctx) x
            |> Rational
            |> \v -> v::s
            |> Ok
        Int x ->
            Numerics.fromInt x
            |> Rational
            |> \v -> v::s
            |> Ok
        Group g ->
            evalGroup ctx g
            |> Result.map (\groupStack -> groupStack ++ s)
        Op o ->
            runOp ctx o s

eval : Context -> Expr -> Result Error (List Value)
eval ctx e =
    evalRec ctx e []