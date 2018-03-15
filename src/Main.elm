module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Kintail.InputWidget as Input
import Hashbow

import Expr exposing (Expr(..))
import Eval exposing (eval, Error(..), Value(..))
import Ops exposing (calcOps, opDocs)

import Numerics
import BigInt
import Dict exposing (Dict(..))

main = Html.beginnerProgram { model = model, update = update, view = view }

type OutputConf = Float | FormattedRational

type alias Model =
    { result : Result Error (List Eval.Value)
    , expression : String
    , outputSetting : OutputConf
    , floatingPointControl : Float
    }

model : Model
model =
    { result = Ok []
    , expression = ""
    , outputSetting = FormattedRational
    , floatingPointControl = 6
    }

type Msg = ExpressionTyped String | SwitchOutputConf OutputConf | AdjustFloatingPointControl Float

-- Converts the slider input to an int suitable for use as the floating point accuracy/precision setting
toFlopControl : Float -> Int
toFlopControl f =
    10 ^ f |> floor

update : Msg -> Model -> Model
update msg model =
    case msg of
        ExpressionTyped str ->
            Expr.parse ("(" ++ str ++ ")") -- wrap str in brackets so it's recognized as a group
            |> Result.mapError ParseError
            |> Result.andThen (eval (Eval.Context calcOps <| toFlopControl model.floatingPointControl))
            |> \r -> { model | result = r, expression = str }
        SwitchOutputConf c ->
            { model | outputSetting = c }
        AdjustFloatingPointControl f ->
            { model | floatingPointControl = f }

-- Pretty-print an Error
error : Error -> Html a
error err =
    let
        prettyPrint e =
            case e of
                MathematicalImpossibility -> "Does not compute"
                OpNotFound o -> String.join " " ["Operator not found: ", o]
                StackUnderflow x -> String.join " " ["Stack underflowed by", toString x, "items"]
                ParseError e -> String.join " " ["Parse error:", e]
    in
        div [class "error"] [text <| prettyPrint err]

-- Renders a Value as an item in a stack
stackItem : OutputConf -> Eval.Value -> Html a
stackItem conf v =
    let
        anyText = toString >> text
        bigintText = BigInt.toString >> text
        center = List.singleton >> div [class "horizontal-center"]
        displayFormattedRational r =
            [ center <| bigintText <| Numerics.numerator r ] ++
            if Numerics.denominator r /= BigInt.fromInt 1 then
                [ hr [] []
                , center <| bigintText <| Numerics.denominator r
                ]
            else []

        inner = case v of
            Rational r -> case conf of
                FormattedRational -> [div [class "formatted-rational"] <| displayFormattedRational r]
                Float -> [anyText <| Numerics.toFloat r]
            Partial (op, stack, needs) -> 
                [div [class "partial"]
                    [ div [class "partial-op"] [text op]
                    , div [class "partial-missing"] [text <| String.join " " ["Missing", toString needs, "values"]]
                    , div [class "partial-stack"] <| List.map (stackItem conf) stack
                    ]]
    in
        div [class "item"] inner

-- The controls for number formatting
formatControl : OutputConf -> Html OutputConf
formatControl c =
    let
        radioButton txt msg = label [] [Input.radioButton [] msg c, text txt]
    in
        div []
            [ radioButton "Rational (formatted)" FormattedRational
            , br [] []
            , radioButton "Decimal (via floating point, may break)" Float
            ]

floatingPointControl : Float -> Html Float
floatingPointControl f =
    let
        slider = Input.slider [class "float-slider"] { min = 1, max = 12, step = 0.5 } f
    in
        label [class "float-slider-label"] [slider, text "Floating Point Accuracy/Precision Adjuster"]

displayDocs : Dict String String -> List (Html a)
displayDocs docs =
    let
        docsL = Dict.toList docs
        entry (op, desc) = div [class "op"] 
            [ span [class "op-name", style [("background-color", Hashbow.hashbow op)]] [text op]
            , span [class "op-desc"] [text desc]
            ]
    in
        List.map entry docsL

view : Model -> Html Msg
view model =
    let stack = 
        case model.result of
            Ok stack ->
                List.reverse stack -- Puts first items at the top, for nicer looks
                |> List.map (stackItem model.outputSetting)
            Err outputError ->
                [error outputError]
    in div [class "rpncalc"] (
        [ div [class "stack"] stack
        , input [onInput ExpressionTyped, value model.expression, class "exprinput", autofocus True, placeholder "Expression"] []
        , div [class "config-panel"] 
            [ formatControl model.outputSetting |> Html.map SwitchOutputConf
            , floatingPointControl model.floatingPointControl |> Html.map AdjustFloatingPointControl
            ]
        , div [class "docs"] (displayDocs Ops.opDocs)
        ]
    )