module Parser exposing (..)

import Char


type alias Parser a =
    String -> Maybe ( a, String )


char0 : Parser Char
char0 =
    String.uncons


run : Parser a -> String -> Maybe a
run p s =
    p s
        |> Maybe.map Tuple.first


r : String -> Parser a -> Maybe a
r =
    flip run



-- parse first character
-- and then
-- if that character is a digit -- succed with the character
-- of not , fail with notning


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation pa =
    \inp ->
        case pa inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing


succeed : a -> Parser a
succeed a =
    \inp ->
        Just ( a, inp )


fail : Parser a
fail =
    \inp -> Nothing


satisfies : (Char -> Bool) -> Parser Char
satisfies predicate =
    char0
        |> andThen
            (\c ->
                if predicate c then
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


toInt : Char -> Int
toInt c =
    Char.toCode c - Char.toCode '0'


intDigit : Parser Int
intDigit =
    digit
        |> andThen (succeed << toInt)


map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser |> andThen (\c -> succeed (f c))


map0 : (a -> b) -> Parser a -> Parser b
map0 f =
    andThen <| succeed << f


intDigit0 : Parser Int
intDigit0 =
    digit
        |> map toInt


intDigit1 : Parser Int
intDigit1 =
    succeed toInt
        |> take digit


take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf |> andThen (\f -> map f pa)


apply : (a -> b) -> a -> b
apply f a =
    f a


map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 f parsera parserb =
    parsera
        |> andThen
            (\a ->
                parserb
                    |> map (\b -> f a b)
            )


map2_ : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2_ f pa pb =
    pa |> andThen (\a -> pb |> andThen (\b -> succeed (f a b)))


take2 : Parser a -> Parser (a -> b) -> Parser b
take2 =
    map2 <| flip apply


drop : Parser drop -> Parser keep -> Parser keep
drop dropper keeper =
    keeper
        |> andThen
            (\value ->
                dropper
                    |> andThen (\_ -> succeed value)
            )


char : Char -> Parser Char
char x =
    satisfies (\c -> c == x)


pair : Parser ( Char, Char )
pair =
    succeed (,)
        |> drop (char '(')
        |> take digit
        |> drop (char ',')
        |> take digit
        |> drop (char ')')


pairN : Parser ( Int, Int )
pairN =
    succeed (,)
        |> drop (char '(')
        |> take intDigit
        |> drop (char ',')
        |> take intDigit
        |> drop (char ')')


either : (b -> Maybe a) -> (b -> Maybe a) -> b -> Maybe a
either pa pb =
    \inp ->
        pa inp
            |> Maybe.map Just
            |> Maybe.withDefault (pb inp)


either_ : (b -> Maybe a) -> (b -> Maybe a) -> b -> Maybe a
either_ pa pb =
    \inp ->
        case pa inp of
            Just res ->
                Just res

            Nothing ->
                pb inp


lazy : (() -> Parser a) -> Parser a
lazy f =
    \inp ->
        f () inp



{-
   digits : Parser (List Int)
   digits =
       either
           (succeed (::)
               |> take intDigit
               |> take digits
           )
           (succeed [])

-}


digits : Parser (List Char)
digits =
    either
        (succeed (::)
            |> take digit
            |> take (lazy (\_ -> digits))
        )
        (succeed [])


many : Parser a -> Parser (List a)
many p =
    either
        (succeed (::)
            |> take p
            |> take (lazy (\_ -> many p))
        )
        (succeed [])


digits0 : Parser (List Char)
digits0 =
    many digit


intFromList : List Int -> Int
intFromList =
    List.foldl (\i a -> a * 10 + i) 0


integer : Parser Int
integer =
    oneOrMore intDigit
        |> map intFromList


oneOrMore : Parser a -> Parser (List a)
oneOrMore p =
    either
        (succeed (::)
            |> take p
            |> take (lazy (\_ -> many p))
        )
        fail
