# Parsers

## What is a parser?

- a function to extract or decode an interesting value from a string

Start with identity function - returns its argument:

```
        \x -> x
```

This could serve as a parser that returns the entire string.
Not especially useful, but a good start.

It exists in Elm as `identity` so we could define a parser as:

```elm
parser: String -> String
parser = identity
```

## More general parsing

In general, however, we may want to:

1. match only specific patterns -- parsing might fail
2. convert the matching string to some other type, e.g. abstract syntax tree
3. not match the entire string -- some will be left over

E.g. say we're interested in the fist character of the input string:

- returns `Char`, not `String`

- also return the unparsed input:

```
    \ (head::tail) -> (head, tail)
```

## Handling failure

- This will fail if the input string is empty:

    - [Hutton and Meijer](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) use List

    - [elm-tools/Parser](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser)
      uses [Result](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Result)

    - I'm using `Maybe`; it keeps the examples short (no error messages) --
      less to type.

**Note**: Strings aren't natively `List Char` in Elm:

```elm
character0 : String -> Maybe (Char, String)
character0 inp =
    case String.toList inp of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, String.fromList tail )
```

**Generalising**: We don't always want `Char`, let's use a type variable:

```elm

type alias Parser a
    = String -> Maybe (a, String)

```

Turns out we can use [uncons](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/String#uncons):

 ```elm
character0 : Parser Char
character0 = String.uncons
 ```

```elm
 > character0 "abc"
 Just ('a',"bc") : Maybe ( Char, String )
```

## Running parsers

`run` ignores any unconsumed input and returns the current value:

```elm
run : Parser a -> String -> Maybe a
run p s =
    p s |> Maybe.map Tuple.first
```

Now we can _run_ `character0` on a given string

```elm
> run character0 "abc"
Just 'a' : Maybe Char
```

## Pattern matching

What if we wanted to :

- match only a particular character or class (digits or letters, etc.)?
- convert to other types.
- fail if not all the input sting was matched?

E.g. lets match only digits (`0-9`) (and, later, convert to `Int`).

We can use `character0` **and then**: **succeed** if its result was a
digit ('0' - '9'), or **fail** otherwise?

### Combine parsers -- `andThen`

This is _bind_ (`>>=`), with its arguments flipped;
Elm style encourages use of `|>` pipes:

```
 (|>) : a -> (a -> b) -> b

```

```elm
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation parser =
    \inp ->
        case parser inp of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing
```

```elm
andThen_ : (a -> Parser b) -> Parser a -> Parser b
andThen_ continuation parser =
    parser >> Maybe.andThen (uncurry continuation)

```

Now we can write:

```
    character0
    |> andThen (\c -> ...do something with char c...)

```

And, what we want to do is test if `c` is a digit:

``` elm
character0
    |> andThen
        (\c ->
            if Char.isDigit c then
                -- parsing succeeds and the result is c
            else
                -- parsing fails
        )

```

### Always succeed

A parser that never fails, and always returns a specific value `val`, and the
entire unaltered input `inp`

```elm
succeed : a -> Parser a
succeed val =
    \inp -> Just ( val, inp )
```

### Always fail

Ignore the input and just fail

```elm
fail : Parser a
fail =
    \inp -> Nothing
```

Now we can write:

```elm
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
```

**Generalising**: test any predicate on the character:

```elm
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
```

Now we can match specific characters or character groups:

```elm
digit : Parser Char
digit =
    satisfies Char.isDigit


upper : Parser Char
upper =
    satisfies Char.isUpper

char : Char -> Parser Char
char c =
    satisfies (\x -> x == c)
```

### Converting to other types

What if we want results of some other type than Char?
e.g. parse a digit and then convert it to an `Int`.

```elm
toInt : Char -> Int
toInt c =
    Char.toCode c - Char.toCode '0'


intDigit : Parser Int
intDigit =
    digit
        |> andThen
            (\a ->
                succeed (toInt a)
            )


intDigit0 : Parser Int
intDigit0 =
    digit |> andThen (succeed << toInt)
```

**Generalising**: this:

- `digit` becomes any parser `p :  Parser a`
- `toInt` becomes any function `f : a -> b`

Applying a function to the value returned by a parser is called `map`.

```elm
map : (a -> b) -> Parser a -> Parser b
map f p =
    p |> andThen (\a -> succeed (f a))
```

Or, more cryptically:

```elm
map : (a -> b) -> Parser a -> Parser b
map f =
    andThen (succeed << f)
```

Now we can convert to other types:

```elm
intDigit1 : Parser Int
intDigit1 =
    digit |> map toInt
```

- what if we want the multi-digit version of this?

## Chose what we keep and what we ignore

### Keep stuff

```elm
take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf
        |> andThen
            (\f ->
                pa
                    |> andThen (\a -> succeed (f a))
             )
```

```elm
take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf |> andThen (\f -> map f pa)
```

Take has its its arguments flipped for infix
use with `|>`.

To use it we need a _parser function_ `pf : Parser (a -> b)`;
a parser that returns a function:

E.g. make a parser that returns  `toInt`:

```elm
> succeed toInt
<function> : Parser.Parser (Char -> Int)
```

And pipe it into `take digit`:

```elm
intDigit1 =
    take digit <| succeed toInt
```

```elm
> mkList = succeed List.singleton
<function> : Parser (a -> List a)
```

Now we can run it in a pipeline:

```elm
> run (mkList |> take digit ) "50"
Just ['5'] : Maybe (List Char)
```

But its more interesting with multi-argument functions like `(,) : a -> b -> (a,b)`

```elm
> mkTuple = succeed (,)
    |> take digit
    |> take digit

> run mkTuple "50"
Just ('5','0') : Maybe ( Char, Char )
```

### Ignore stuff

`drop` is a parser that applies another parser
and ignores its output.
The other parser must match, however.

```elm
drop : Parser drop -> Parser keep -> Parser keep
drop dropper keeper =
    keeper
        |> andThen
            (\value ->
                dropper
                    |> andThen (\_ -> succeed value)
            )
```

It works like this:

```
> (upper |> drop digit ) "A5"
Just ('A',"") : Maybe.Maybe ( Char, String )
```

Which seems odd, but wait:

Now we can chose what to `take`, and what to `drop`:

```elm
pair : Parser ( Char, Char )
pair =
    succeed (,)
        |> drop (char '(')
        |> take character
        |> drop (char ',')
        |> take character
        |> drop (char ')')
```

```elm
> pair "(a,b)"
Just (('a','b'),"") : Maybe.Maybe ( ( Char, Char ), String )
```


## Parser pipelines

`elm-tools/Parser` does this with its own infix [pipe operators](https://github.com/elm-tools/parser#parser-pipelines):

```elm
apply : (a -> b) -> a -> b
apply f a =
    f a

-- This is take
infixl 5 |=
(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) pf pa =
    map2 apply pf pa


-- this is drop
infixl 5 |.
(|.) : Parser keep -> Parser ignore -> Parser keep
(|.) keeper ignorer =
    map2 always keeper ignorer
```

So we can write

```elm
pair1 : Parser ( Int, Int )
pair1 =
    succeed (,)
        |. char '('
        |= num
        |. char ','
        |= num
        |. char ')'
        |. end
```

```elm
> run pair1 "(5,0)"
Just (5,0) : Maybe ( Int, Int )
```

### Repetition

Suppose we want to take several digits?

To parse a list of things, we need

```elm
many : Parser a -> Parser (List a)
```

**Either** the string contains an `a` value,
followed by `many` (possibly zero) more `a` values,
**or** there are none.

```
    many parser =
        either
            (succeed (::)
                |= parser
                |= many parser
            )
            (succeed [])
```

So let's define `either`:

```elm
either : Parser a -> Parser a -> Parser a
either p1 p2 =
    \inp ->
        case p1 inp of
            (Just _) as good ->
                good

            Nothing ->
                p2 inp

```

```elm
either : Parser a -> Parser a -> Parser a
either parser1 parser2 =
    \inp ->
        parser1 inp -- gives a `Maybe`
        |> Maybe.map Just -- gives a `Maybe` of a `Maybe`
        |> Maybe.withDefault (parser2 inp)
            -- strips off one `Maybe`,
            -- but the default IS a `Maybe`
```

Then use `either` to define `many`:

```elm
many : Parser a -> Parser (List a)
many parser =
    either
        (succeed (::)
            |> take parser
            |> take many parser -- uh oh
        )
        (succeed [])
```

This blows up because `many` appears on the left and right of the
definition. The solution is to make the recursive call to `many`
lazy so it doesn't get evaluated unless we need it. (Elm is eager)

```elm
lazy : (() -> Parser a) -> Parser a
lazy lazyfunc =
    \inp -> lazyfunc () inp
```

Generalizing:

```elm
lazy : (() -> (a -> b)) -> a -> b
lazy f =
    \a -> f () a
```

```elm
many : Parser a -> Parser (List a)
many parser =
    either
        (succeed (::)
            |> take parser
            |> take (lazy (\() -> many parser)) -- Elm is not lazy
        )
        (succeed [])
```

```
> run (many digit) "123abc"
Just ['1','2','3'] : Maybe.Maybe (List Char)
```

Now we can get all available digits and turn them into an integer

```elm
integer : Parser Int
integer =
    take (many digit) <|
        succeed
            (String.fromList
                >> String.toInt
                >> Result.withDefault 0 -- I am lazy!
            )
```

Defaulting to 0 is not a smart idea, we should really require at least 1 digit,
and make the parser fail, but enough is enough.

```elm
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
```

```elm
> run integer "123abc"
Just 123 : Maybe.Maybe Int

> run integer "abc"
Nothing : Maybe.Maybe Int
```

## Detecting the end of input

We can detect the end of the input string:

```elm
end : Parser ()
end =
    \inp ->
        if String.isEmpty inp then
            Just ( (), inp )
        else
            Nothing
```

And ignore the unit `()` with `drop`:

```elm
justA : Parser Char
justA =
    char 'A'
        |> drop end
```

```elm
> run justA "a"
Nothing : Maybe Char

> run justA "A"
Just 'A' : Maybe Char

> run justA "AA"
Nothing : Maybe Char
```

