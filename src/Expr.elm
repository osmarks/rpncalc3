module Expr exposing (Expr(..), Op(..), parse)

import Combine exposing (..)
import Combine.Num as Num

type Op
    = Add
    | Subtract
    | Multiply
    | Divide

type Expr 
    = Num Float
    | Op Op
    | Group (List Expr)

num : Parser () Expr
num =
    Num <$> Num.float <|> Num << toFloat <$> Num.int

stringIs : String -> a -> Parser s a
stringIs str val =
    string str *> succeed val

op : Parser () Expr
op =
    stringIs "+" Add
    <|> stringIs "-" Subtract
    <|> stringIs "*" Multiply
    <|> stringIs "/" Divide
    |> map Op

group : Parser () Expr
group =
    between (string "(") (string ")") (sepBy1 whitespace (lazy <| \_ -> parser)) -- Avoid bad recursion issues using lazy parser evaluation
    |> map Group

parser : Parser () Expr
parser =
    (lazy <| \_ -> group) <|> op <|> num

parse : String -> Result (ParseErr ()) (ParseOk () Expr)
parse =
    Combine.parse parser