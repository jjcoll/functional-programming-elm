module Caesar exposing (..)

{-| The number of letters in the alphabet.
Calculated by finding the distance between 'z' and 'a' and adding 1 to
include the starting letter (Fencepost Error prevention).
-}


alphabetLength : Int
alphabetLength =
    (Char.toCode 'z' - Char.toCode 'a') + 1


{-| Helper function to calculate the new character position.

1.  Normalises the character to a 0-25 range.
2.  Applies the key shift.
3.  Uses modBy to wrap around the alphabet[cite: 51].
4.  Re-applies the ASCII offset (base) to return to a valid ASCII code.

-}
applyOffset : Int -> Int -> Char -> Int
applyOffset characterCode key base =
    modBy alphabetLength ((characterCode - Char.toCode base) + key) + Char.toCode base


{-| Encrypts a single character using a key.
Respects casing and ignores non-letter characters.
-}
encode : Int -> Char -> Char
encode key character =
    let
        code =
            Char.toCode character
    in
    if Char.isLower character then
        -- apply calculation
        Char.fromCode (applyOffset code key 'a')

    else if Char.isUpper character then
        -- apply calculation
        Char.fromCode (applyOffset code key 'A')

    else
        character
