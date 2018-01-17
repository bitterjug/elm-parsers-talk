module Parsers exposing (..)

import String
import Tuple


type alias Parser a =
    String -> Maybe ( a, String )


character0 : Parser Char
character0 =
    String.uncons


run : Parser a -> String -> Maybe a
run p s =
    p s |> Maybe.map Tuple.first


succeed : a -> Parser a
succeed val =
    \inp -> Just ( val, inp )


fail : Parser a
fail =
    \inp -> Nothing


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation parser =
    \inp ->
        case parser inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing


satisfies : (Char -> Bool) -> Parser Char
satisfies pred =
    character0
        |> andThen
            (\char ->
                if pred char then
                    succeed char
                else
                    fail
            )
