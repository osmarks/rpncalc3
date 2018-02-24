module Expr exposing (Expr(..), OpName, parse)

import Combine exposing (..)
import Combine.Num as Num
import Combine.Char as Char

type alias OpName = String

type Expr 
    = Num Int
    | Op OpName
    | Group (List Expr)

num : Parser () Expr
num =
    (Num <$> Num.int)
    <?> "expected a number (int)"

acceptableOperatorName : Parser () String
acceptableOperatorName =
    regex "[A-Za-z\\^%*$£!@#~.,=+-_;:/\\\\]*"

op : Parser () Expr
op =
    acceptableOperatorName
    |> map Op

group : Parser () Expr
group =
    parens (sepBy1 whitespace (lazy <| \_ -> parser)) -- Avoid "bad recursion" issues using lazy parser evaluation
    <?> "expected a group (whitespace-separated expressions between brackets)"
    |> map Group

parser : Parser () Expr
parser =
    (lazy <| \_ -> group) <|> num <|> op

convertErrorList : List String -> String
convertErrorList =
    List.intersperse " or " >> String.concat

parse : String -> Result String Expr
parse =
    Combine.parse parser
    >> Result.mapError (\(_, _, errorList) -> convertErrorList errorList) -- Convert error data into only the error message
    >> Result.map (\(_, _, data) -> data) -- Drop irrelevant parse data