module Parser exposing (..)

import String
import Char


type alias Parser a =
    String -> Maybe ( a, String )


fail : Parser a
fail =
    \inp -> Nothing


succeed : a -> Parser a
succeed val =
    \inp -> Just ( val, inp )


character0 : Parser Char
character0 =
    \inp ->
        case String.toList inp of
            [] ->
                Nothing

            c :: cs ->
                Just ( c, String.fromList cs )


character : Parser Char
character =
    String.uncons


run : Parser a -> String -> Maybe a
run p s =
    p s |> Maybe.map Tuple.first


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation parser =
    \inp ->
        case parser inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing


andThen1 : (a -> Parser b) -> Parser a -> Parser b
andThen1 continuation parser =
    \inp ->
        parser inp
            |> Maybe.andThen
                (\( aVal, rest ) ->
                    continuation aVal rest
                )


satisfies : (Char -> Bool) -> Parser Char
satisfies pred =
    character
        |> andThen
            (\char ->
                if pred char then
                    succeed char
                else
                    fail
            )


char : Char -> Parser Char
char c =
    satisfies (\x -> x == c)


digit : Parser Char
digit =
    satisfies Char.isDigit


upper : Parser Char
upper =
    satisfies Char.isUpper


justA : Parser Char
justA =
    char 'A'
        |> drop end



-- There are many suitable predicates in Char, isUpper, isDigit ,etc.


end : Parser ()
end =
    \inp ->
        if String.isEmpty inp then
            Just ( (), inp )
        else
            Nothing


map0 : (a -> b) -> Parser a -> Parser b
map0 f parser =
    \inp ->
        case parser inp of
            Just ( v, rest ) ->
                Just ( f v, rest )

            Nothing ->
                Nothing


map : (a -> b) -> Parser a -> Parser b
map f parser =
    \inp ->
        parser inp
            |> Maybe.map (\( v, rest ) -> ( f v, rest ))


map_ : (a -> b) -> Parser a -> Parser b
map_ f p =
    p |> andThen (\a -> succeed (f a))


map2 : (a -> b -> val) -> Parser a -> Parser b -> Parser val
map2 f pa pb =
    pa |> andThen (\a -> pb |> andThen (\b -> succeed (f a b)))


num : Parser Int
num =
    digit
        |> map String.fromChar
        |> map String.toInt
        |> map (Result.withDefault 0)


apply : (a -> b) -> a -> b
apply f a =
    f a


infixl 5 |=
(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) pf pa =
    map2 apply pf pa


infixl 5 |.
(|.) : Parser keep -> Parser ignore -> Parser keep
(|.) keeper ignorer =
    map2 always keeper ignorer


drop : Parser drop -> Parser keep -> Parser keep
drop dropper keeper =
    keeper
        |> andThen
            (\value ->
                dropper
                    |> andThen (\_ -> succeed value)
            )


take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf
        |> andThen
            (\f ->
                pa
                    |> andThen (\a -> succeed (f a))
            )


pair : Parser ( Char, Char )
pair =
    succeed (,)
        |> drop (char '(')
        |> take character
        |> drop (char ',')
        |> take character
        |> drop (char ')')
        |> drop end


either : Parser a -> Parser a -> Parser a
either p1 p2 =
    \inp ->
        case p1 inp of
            (Just _) as good ->
                good

            Nothing ->
                p2 inp


lazy : (() -> Parser a) -> Parser a
lazy thunk =
    \inp -> thunk () inp


many : Parser a -> Parser (List a)
many parser =
    either
        (succeed (::)
            |> take parser
            |> take (lazy (\() -> many parser))
        )
        (succeed [])


integer : Parser Int
integer =
    take (many digit) <|
        succeed
            (String.fromList
                >> String.toInt
                >> Result.withDefault 0
            )


test1 : Maybe ( Char, Char )
test1 =
    run pair "(a,b)xx"


test2 : Maybe ( Char, Char )
test2 =
    run pair "(a,b)"


pair1 : Parser ( Int, Int )
pair1 =
    succeed (,)
        |. char '('
        |= num
        |. char ','
        |= num
        |. char ')'
        |. end
