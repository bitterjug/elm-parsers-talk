module Parser exposing (..)

import Char


{-
    \ a -> a

    - a parser that returns the whole input string

   3 problems:
     - always succeeds -- parsing might fail if the input string isn't waht we are expecting
     - always consumes enire input -- we might want to parse the beginning and leave the rest
     - only returns a string -- we might want other types
-}


{-|

       \ (c::cs) -> (c, cs)

strings are not natively lists of characters in Elm

-}
char0 : String -> Maybe ( Char, String )
char0 inp =
    case String.toList inp of
        c :: cs ->
            Just ( c, String.fromList cs )

        [] ->
            Nothing


type alias Parser a =
    String -> Maybe ( a, String )


run : Parser a -> String -> Maybe a
run p inp =
    Maybe.map Tuple.first <| p inp


character0 : Parser Char
character0 =
    String.uncons



{- parse only digits -- convert to int

   parse a character and then succeed if its a digit, fail otherwise

    c := character0
    if Char.isDigit c then
        succeed c
    else
        fail -- with an error message


    character0 |> andThen
            \c
                if  Char.isDigit c then
                    succeed c
                else
                    fail
-}


fail : Parser a
fail =
    \inp -> Nothing


succeed : a -> Parser a
succeed value =
    \inp -> Just ( value, inp )


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation pa =
    pa >> Maybe.andThen (uncurry continuation)


andThen_ : (a -> Parser b) -> Parser a -> Parser b
andThen_ continuation pa =
    \inp ->
        case pa inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing


parseDigit : Parser Char
parseDigit =
    character0
        |> andThen
            (\c ->
                if Char.isDigit c then
                    succeed c
                else
                    fail
            )


satisfies : (Char -> Bool) -> Parser Char
satisfies pred =
    character0
        |> andThen
            (\c ->
                if pred c then
                    succeed c
                else
                    fail
            )


digit : Parser Char
digit =
    satisfies Char.isDigit


upper : Parser Char
upper =
    satisfies Char.isUpper


char : Char -> Parser Char
char c =
    satisfies ((==) c)



{- what if we want to convert that digit character to Int? -}


toInt : Char -> Int
toInt c =
    Char.toCode c - Char.toCode '0'


intDigit : Parser Int
intDigit =
    digit |> andThen (succeed << toInt)



{- generalise this -}


map : (a -> b) -> Parser a -> Parser b
map f =
    andThen (succeed << f)


map_ : (a -> b) -> Parser a -> Parser b
map_ f p =
    p |> andThen (\a -> succeed <| f a)


intDigit_ : Parser Int
intDigit_ =
    digit |> map toInt


intDigit1 : Parser Int
intDigit1 =
    take digit <| succeed toInt


either : Parser a -> Parser a -> Parser a
either parser1 parser2 =
    \inp ->
        parser1 inp
            |> Maybe.map Just
            |> Maybe.withDefault (parser2 inp)


drop : Parser drop -> Parser keep -> Parser keep
drop dropper keeper =
    keeper
        |> andThen
            (\valueToKeep ->
                dropper
                    |> andThen (\_ -> succeed valueToKeep)
            )


drop_ : Parser drop -> Parser keep -> Parser keep
drop_ dropper keeper =
    keeper
        |> andThen
            (\valueToKeep ->
                dropper
                    |> andThen (always <| succeed valueToKeep)
            )


take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf
        |> andThen
            (\f ->
                pa |> andThen (\a -> succeed (f a))
            )


pair : Parser ( Char, Char )
pair =
    succeed (,)
        |> drop (char '(')
        |> take character0
        |> drop (char ',')
        |> take character0
        |> drop (char ')')


lazy : (() -> (a -> b)) -> a -> b
lazy f a =
    f () a


either_ : Parser a -> Parser a -> Parser a
either_ p1 p2 =
    \inp ->
        case p1 inp of
            Nothing ->
                p2 inp

            good ->
                good


many : Parser a -> Parser (List a)
many parser =
    either_
        (succeed (::)
            |> take parser
            |> take (lazy (\() -> many parser))
        )
        (succeed [])


listToInt : List Char -> Int
listToInt =
    String.fromList
        >> String.toInt
        >> Result.withDefault 0


integer_ =
    take (many digit) <|
        succeed listToInt


integer : Parser Int
integer =
    many digit
        |> andThen
            (\list ->
                list
                    |> String.fromList
                    |> String.toInt
                    |> Result.map succeed
                    |> Result.withDefault fail
            )
