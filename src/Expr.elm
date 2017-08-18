module Expr exposing (Expr(..), Op(..), parse)

import Combine exposing (..)
import Combine.Num as Num

type Op
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponent
    | Range

type Expr 
    = Num Float
    | Op Op
    | Group (List Expr)

num : Parser () Expr
num =
    (Num <$> Num.float <|> Num << toFloat <$> Num.int)
    <?> "expected a number (int or float)"

stringIs : String -> a -> Parser s a
stringIs str val =
    string str *> succeed val

op : Parser () Expr
op =
    stringIs "+" Add
    <|> stringIs "-" Subtract
    <|> stringIs "*" Multiply
    <|> stringIs "/" Divide
    <|> stringIs "^" Exponent
    <|> stringIs "range" Range
    |> map Op

group : Parser () Expr
group =
    between (string "(") (string ")") (sepBy1 whitespace (lazy <| \_ -> parser)) -- Avoid bad recursion issues using lazy parser evaluation
    <?> "expected a group (whitespace-separated expressions between brackets)"
    |> map Group

parser : Parser () Expr
parser =
    (lazy <| \_ -> group) <|> op <|> num

parse : String -> Result (List String) Expr
parse =
    Combine.parse parser
    >> Result.mapError (\(_, _, errorList) -> errorList)
    >> Result.map (\(_, _, expr) -> expr) -- Convert errors/results to nicer format