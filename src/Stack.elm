-- Contains functions for use of a list as a stack, specifically for use in the calculator
module Stack exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..))

-- Gets the specified number of predicate-satisfying values from supplied stack. Returns new stack and values collected
pick : (a -> Bool) -> List a -> Int -> Result Int (List a, Nonempty a)
pick pred stack qty =
    let pick curr (values, newStack, num) =
            if pred curr then (curr::values, newStack, num + 1)
            else (values, curr::newStack, num)

        foldF = \c (val, new, num) ->
            if num < qty then pick c (val, new, num)
            else (val, c::new, num)
        (values, newStack, num) = List.foldl foldF ([], [], 0) stack
    in 
        if num == qty then
            Nonempty.fromList values
            |> Result.fromMaybe qty
            |> Result.map (\v -> (newStack, v))
        else
            Err <| qty - num

pop : List a -> Maybe (a, List a)
pop l = 
    List.head l |> Maybe.map (\h -> (h, List.tail l |> Maybe.withDefault []))