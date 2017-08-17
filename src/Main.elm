module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Expr exposing (Expr(..), Op(..))

import Stack exposing (Stack(..))

main = Html.beginnerProgram { model = model, update = update, view = view }

type alias Model =
    { result : Result (List String) (List Float)
    , expression : String
    }

model : Model
model =
    { result = Ok []
    , expression = ""
    }

type Msg = ExpressionTyped String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ExpressionTyped str ->
            Expr.parse ("(" ++ str ++ ")") -- wrap str in brackets so it's recognized as a group
            |> Result.andThen eval -- Convert stack underflow errors into a list
            |> \r -> { model | result = r, expression = str }

error : String -> Html a
error err =
    div [class "error"] [text err]

stackItem : Float -> Html a
stackItem n =
    let asString = toString n
        minWidth = toString (String.length asString) ++ "em"
    in div [class "item", style [("min-width", minWidth)]] [text asString]

view : Model -> Html Msg
view model =
    let calcOutput = 
        case model.result of
            Ok stack ->
                List.reverse stack -- Puts first items at the top, for nicer looks
                |> List.map stackItem
            Err errors ->
                List.map error errors
    in div [class "rpncalc"] (
        [ input [onInput ExpressionTyped, value model.expression, class "exprinput"] []
        ] ++ calcOutput
    )

-- Run a binary operation on a stack - returns updated stack or an error
binOp : (Float -> Float -> Float) -> Stack Float -> Result String (Stack Float)
binOp f s =
    let (maybeX, s1) = Stack.pop s
        (maybeY, s2) = Stack.pop s1
        maybeXY = Maybe.map2 (,) maybeX maybeY
        result = Maybe.map (\(x, y) -> f y x) maybeXY -- x and y swapped round - this makes "5 1 /" become 5 instead of 0.2.
        finalStack = Maybe.map (\r -> Stack.push r s2) result
    in Result.fromMaybe "Stack underflow" finalStack

prepend : Stack a -> Stack a -> Stack a
prepend from to =
    Stack.toList from
    |> List.foldr Stack.push to

evalRec : Expr -> Stack Float -> Result String (Stack Float)
evalRec expr s =
    case expr of
        Group es ->
            List.foldl (\expr s -> Result.andThen (evalRec expr) s) (Ok Stack.initialise) es
            |> Result.map (\newStack -> prepend newStack s) -- prepend new stack's contents to old stack
        Num n ->
            Ok (Stack.push n s)
        Op o ->
            case o of
                Add -> binOp (+) s
                Subtract -> binOp (-) s
                Multiply -> binOp (*) s
                Divide -> binOp (/) s

eval : Expr -> Result (List String) (List Float)
eval e =
    evalRec e (Stack.initialise)
    |> Result.map Stack.toList -- Convert stack to list
    |> Result.mapError List.singleton -- Wrap possible stackoverflow error in list