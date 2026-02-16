module Pythagoras exposing (..)

import String exposing (cons)



-- Pythagoras triple is set of 3 positive integers a, b, c
-- that satisfy a^2+b^2=c^2


sqr : Int -> Int
sqr int =
    int * int


isTripple : Int -> Int -> Int -> Bool
isTripple a b c =
    a > 0 && b > 0 && c > 0 && (sqr a + sqr b == sqr c)


leg1 : Int -> Int -> Int
leg1 x y =
    sqr x - sqr y


leg2 : Int -> Int -> Int
leg2 x y =
    2 * x * y


hyp : Int -> Int -> Int
hyp x y =
    sqr x + sqr y


isTrippleTuple : ( Int, Int, Int ) -> Bool
isTrippleTuple ( x, y, z ) =
    isTripple x y z



-- generates pythagorean triples given both x & y > 0 & x > y


pythTripple : ( Int, Int ) -> ( Int, Int, Int )
pythTripple ( x, y ) =
    ( leg1 x y, leg2 x y, hyp x y )


pythTriplesMap : List ( Int, Int ) -> List ( Int, Int, Int )
pythTriplesMap list =
    List.map pythTripple list


pythTriplesRec : List ( Int, Int ) -> List ( Int, Int, Int )
pythTriplesRec list =
    -- Lists behave as Linked Lists: Each "Node" is a pair containing a Value (Head) and a Reference (Tail)
    -- to the rest of the list. The last Node points to an empty list []
    case list of
        -- BASE CASE: empty node ([])
        -- Returning [] here provides the 'tail' for the very last triple.
        [] ->
            []

        -- RECURSIVE STEP: Destructuring (Unpacking)
        -- 'head' is the value of the current Node.
        -- 'tail' is the pointer to the NEXT Node (the rest of the list).
        head :: tail ->
            -- RECONSTRUCTING: The (::) operator is O(1)
            -- 1. Transform the current head into a triple.
            -- 2. Create a NEW Node in memory.
            -- 3. Set the 'next' pointer of this new node to the result of 'pythTriplesRec tail'.
            -- 4. This uses "Structural Sharing"—the original tail isn't copied, just referenced.
            pythTripple head :: pythTriplesRec tail


arePythTriplesFilter : List ( Int, Int, Int ) -> List ( Int, Int, Int )
arePythTriplesFilter list =
    List.filter isTrippleTuple list


arePythTriplesRec : List ( Int, Int, Int ) -> List ( Int, Int, Int )
arePythTriplesRec list =
    case list of
        [] ->
            []

        head :: tail ->
            if isTrippleTuple head then
                -- keep head if pythagorean triple
                head :: arePythTriplesRec tail

            else
                -- skip head & keep processing
                arePythTriplesRec tail
