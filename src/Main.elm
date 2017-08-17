module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Html.beginnerProgram { model = model, update = update, view = view }

type alias Model =
    { expression : String
    , result : List Float
    }

model : Model
model = Model "" []

type Msg = ExpressionTyped String

update : Msg -> Model -> Model
update msg model =
    model

view : Model -> Html Msg
view model =
    div [] []