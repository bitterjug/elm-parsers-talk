module Parser exposing (..)

import Char


type alias Parser a =
    String -> Maybe ( a, String )


char0 : Parser Char
char0 =
    String.uncons


run : String -> Parser a -> Maybe a
run s p =
    p s
        |> Maybe.map Tuple.first



{- parse first character with char0
   and then
       if its a digit
           succeed with that char
       else
           fail
-}


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation parser =
    \inp ->
        case parser inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing


digit : Parser Char
digit =
    char0
        |> andThen
            (\c ->
                if Char.isDigit c then
                    succeed c
                else
                    fail
            )


succeed : a -> Parser a
succeed val =
    \inp ->
        Just ( val, inp )


fail : Parser a
fail =
    \inp ->
        Nothing


satisfies : (Char -> Bool) -> Parser Char
satisfies pred =
    char0
        |> andThen
            (\c ->
                if pred c then
                    succeed c
                else
                    fail
            )


digit_ : Parser Char
digit_ =
    satisfies Char.isDigit


upper : Parser Char
upper =
    satisfies Char.isUpper


char : Char -> Parser Char
char c =
    satisfies (\x -> x == c)


toInt : Char -> Int
toInt c =
    Char.toCode c - Char.toCode '0'


intDigit : Parser Int
intDigit =
    digit
        |> andThen (succeed << toInt)


map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser
        |> andThen (succeed << f)


intDigit_ : Parser Int
intDigit_ =
    digit |> map toInt


take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf |> andThen (\f -> map f pa)


intDigit1 : Parser Int
intDigit1 =
    succeed toInt
        |> take digit


either : Parser a -> Parser a -> Parser a
either p1 p2 =
    \inp ->
        case p1 inp of
            Just result ->
                Just result

            Nothing ->
                p2 inp


lazy : (() -> a -> b) -> (a -> b)
lazy f =
    \a -> f () a


digits : Parser (List Char)
digits =
    either
        (succeed (::)
            |> take digit
            |> take (lazy (\() -> digits))
        )
        (succeed [])
