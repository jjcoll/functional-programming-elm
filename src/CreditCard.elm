module CreditCard exposing (..)

-- LUHN CHECKSUM ALGORITHM:
-- 1. Double the value of every second digit starting with right most.
-- 2. Add the digits (e.g. 16 becomes 1 6) of doubled values and undoubled digits.
-- 3. Calculate remainder of the sum divided by 10.
-- 4. If result of mod sum 10 = 0, then valid.
-- RECURSIVE DIGIT SUM
-- Splits a string into characters and sums their integer values recursively.
-- This handles two-digit results from doubling (e.g., "16" becomes 1 + 6 = 7).


sumDigitsOfString : String -> Int
sumDigitsOfString string =
    case String.uncons string of
        -- Base Case: Empty string results in a sum of 0.
        Nothing ->
            0

        -- Recursive Step: Process the head character and recurse on the tail.
        Just ( head, tail ) ->
            case String.toInt (String.fromChar head) of
                Nothing ->
                    sumDigitsOfString tail

                Just int ->
                    int + sumDigitsOfString tail



-- LIST FOLDING SUM
-- Uses a fold-left to accumulate the sum of digits for every string in a list.


sumDigitsOfStringList : List String -> Int
sumDigitsOfStringList list =
    List.foldl (\string acc -> acc + sumDigitsOfString string) 0 list



-- CHARACTER TRANSFORMATION HELPER
-- Converts a single digit character to an integer, doubles it, and returns it as a string.


doubleCharacter : Char -> String
doubleCharacter char =
    case String.toInt (String.fromChar char) of
        Just int ->
            String.fromInt (int * 2)

        Nothing ->
            String.fromChar char



-- POSITIONAL DOUBLER
-- Maps through the list of characters using their indices.
-- It doubles characters at odd-numbered indices (1, 3, 5...), which represent
-- "every second digit" in a reversed list.
-- you can use first :: second :: last TODO: change to this


doubleOddCharacters : List Char -> List String
doubleOddCharacters list =
    List.indexedMap
        (\i char ->
            -- i == 1, 3, 5... are the "second" digits starting from the rightmost.
            if modBy 2 i == 1 then
                doubleCharacter char

            else
                String.fromChar char
        )
        list



-- LIST REVERSER (Fold Implementation)
-- Reverses a list by prepending elements from the left onto an accumulator.
-- This ensures we don't use the forbidden List.reverse function.


reverseCharList : List Char -> List Char
reverseCharList list =
    List.foldl (\element acc -> element :: acc) [] list



-- MAIN VALIDATION FUNCTION
-- Combines the 16-character length check with the Luhn checksum pipeline.


isValid : String -> Bool
isValid cardnumber =
    let
        -- 1. Check if length is exactly 16
        hasCorrectLength =
            String.length cardnumber == 16

        -- 2. Convert string to a list of characters
        charList =
            String.toList cardnumber

        -- 3. Reverse the list (starting from the rightmost digit)
        reversedList =
            reverseCharList charList

        -- 4. Double every second digit (starting with index 1)
        doubledList =
            doubleOddCharacters reversedList

        -- 5. Calculate the final sum of all digits
        totalSum =
            sumDigitsOfStringList doubledList

        -- 6. Check if the sum is divisible by 10
        isChecksumValid =
            modBy 10 totalSum == 0
    in
    -- Final result: Both length and checksum must be True
    hasCorrectLength && isChecksumValid
