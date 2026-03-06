module Lesson exposing (..)


sum : List Int -> Int
sum numbers =
    case numbers of
        [] ->
            0

        x :: xs ->
            x + sum xs



-- List.foldr (+) 0 [3, 2, -1, 5], this does the same as the code above,
-- function - neutral elemenent - list


prod : List Int -> Int
prod numbers =
    case numbers of
        [] ->
            1

        x :: xs ->
            x * sum xs
