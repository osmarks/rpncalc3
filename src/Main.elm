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

listToStack : List a -> Stack a
listToStack =
    List.foldr Stack.push Stack.initialise

prependList : List a -> Stack a -> Stack a
prependList from to =
    List.foldr Stack.push to from

prepend : Stack a -> Stack a -> Stack a
prepend from to =
    prependList (Stack.toList from) to

type alias StackFunction = Stack Float -> Result String (Stack Float)

-- Runs a binary operation which returns a list on a stack
binListOutOp : (Float -> Float -> List Float) -> StackFunction
binListOutOp f s =
    let
        (maybeX, s1) = Stack.pop s
        (maybeY, s2) = Stack.pop s1
    in Maybe.map2 (,) maybeX maybeY
       |> Maybe.map (\(x, y) -> f y x) -- x and y swapped round - this makes "5 1 /" become 5 instead of 0.2.
       |> Maybe.map (\r -> prependList r s2)
       |> Result.fromMaybe "Stack underflow"

-- Runs a binary operation on a stack
binOp : (Float -> Float -> Float) -> StackFunction
binOp f =
    binListOutOp (\x y -> f x y |> List.singleton)

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
                Exponent -> binOp (^) s
                Range ->
                    binListOutOp (\x y -> 
                        List.range (floor x) (floor y)
                        |> List.map toFloat) s

eval : Expr -> Result (List String) (List Float)
eval e =
    evalRec e (Stack.initialise)
    |> Result.map Stack.toList -- Convert stack to list
    |> Result.mapError List.singleton -- Wrap possible stackoverflow error in list