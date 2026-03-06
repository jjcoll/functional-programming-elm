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


decode : Int -> Char -> Char
decode key char =
    encode (0 - key) char



-- Week 2
{-
   Removes all non alphanumerica characters from a string,
   not allowed to use String.filter for implementation
-}


normalize : String -> String
normalize string =
    -- returns Maybe tuple, handle with case of & Just and Nothing
    case String.uncons string of
        -- Base Case: When the string is empty, return an empty string to end the recursion
        Nothing ->
            ""

        -- Recursive Step: Process the first character (head) and continue with the rest (tail).
        Just ( head, tail ) ->
            if Char.isAlpha head then
                -- Keep the character and "cons" it onto the result of the recursive call.
                String.cons head (normalize tail)

            else
                -- Discard the non-letter character and move to the next step.
                normalize tail


encrypt : Int -> String -> String
encrypt key string =
    case String.uncons string of
        Nothing ->
            ""

        Just ( head, tail ) ->
            -- no need to check for alpha characters as encrypt handles it already
            String.cons (encode key head) (encrypt key tail)



-- Week 3


decrypt : Int -> String -> String
decrypt key string =
    encrypt (0 - key) string


isPrefix : List Char -> List Char -> Bool
isPrefix sub full =
    -- SUBSTRING MATCHER (Helper)
    -- Recursively checks if 'sub' list is found at the very start of the 'full' list
    case ( sub, full ) of
        -- empty sub means is prefix return true
        ( [], _ ) ->
            True

        -- empty full means no characters left to compare return false
        ( _, [] ) ->
            False

        -- if head of both match recursive call with tails, else return false no need to keep comparing
        ( hSub :: tSub, hFull :: tFull ) ->
            if hSub == hFull then
                isPrefix tSub tFull

            else
                False


recursiveSearch : List Char -> List Char -> Bool
recursiveSearch sub full =
    -- sliding window scanner, moves through the 'full' list character by character, attempting an isPrefix match at each step.
    if isPrefix sub full then
        True

    else
        case full of
            [] ->
                False

            _ :: tail ->
                recursiveSearch sub tail


myContains : String -> String -> Bool
myContains sub full =
    -- we could avoid this function, but this way we avoid converting to a list on every recursive repetition
    recursiveSearch (String.toList sub) (String.toList full)


containsCanary : List String -> String -> Bool
containsCanary canaries text =
    -- Returns True if at least one string from the canaries list is present in the text.
    let
        filteredList =
            List.filter (\canary -> myContains canary text) canaries
    in
    not (List.isEmpty filteredList)


candidates : List String -> String -> List ( Int, String )
candidates canaries encryptedText =
    -- base function creates return type, we then filter through this list
    let
        keys =
            List.range 1 25

        listEncryptedText =
            List.map (\key -> ( key, decrypt key encryptedText )) keys
    in
    List.filter (\( _, str ) -> containsCanary canaries str) listEncryptedText
