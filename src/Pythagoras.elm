module Pythagoras exposing (..)

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


pythTripple : ( Int, Int ) -> ( Int, Int, Int )
pythTripple ( x, y ) =
    ( leg1 x y, leg2 x y, hyp x y )
