module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Expr exposing (Expr(..), OpName)
import Eval exposing (eval)

import Ratio

main = Html.beginnerProgram { model = model, update = update, view = view }

type alias Model =
    { result : Result String (List Eval.Value)
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
            |> Result.andThen (eval >> Result.mapError Eval.prettyPrintError)
            |> \r -> { model | result = r, expression = str }

error : String -> Html a
error err =
    div [class "error"] [text err]

stackItem : Eval.Value -> Html a
stackItem n =
    let asString = Eval.prettyPrintValue n
        minWidth = toString (String.length asString) ++ "em"
    in div [class "item", style [("min-width", minWidth)]] [text asString]

view : Model -> Html Msg
view model =
    let calcOutput = 
        case model.result of
            Ok stack ->
                List.reverse stack -- Puts first items at the top, for nicer looks
                |> List.map stackItem
            Err outputError ->
                [error outputError]
    in div [class "rpncalc"] (
        [ div [] calcOutput
        , input [onInput ExpressionTyped, value model.expression, class "exprinput"] []
        ]
    )