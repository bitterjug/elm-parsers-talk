# Parsers in Elm

I have  adapted this article from a talk I gave in 2018 where I live-coded
a parser combinator library in Elm from first principles. Bear this in
mind when you look at the code examples. I make choices to keep typing to
a minimum. In a real parser library, things would look different.
For example, I choose `Maybe a`  to encode the possibility of failure.
But `Result error a` would enable the parser to return errors that explain why
parsing has failed.

## What is a parser?

To kick things off, let's say a parser is a function to
extract or decode an interesting value from a string.
Here is the identity function; it returns its argument:

```
\x -> x
```

This might serve as a rudimentary parser if were interested in the entire
string.  It exists in Elm as `identity`, so we could define a parser as:

```elm
parser : String -> String
parser = identity
```

In general, however, we will want to build parsers that:

1. match only specific patterns in the input string
   (i.e. parsing may fail if those patterns don't match)
2. convert the matching part of the string to some other, more interesting type
   (e.g., building an abstract syntax tree from its concrete syntax)
3. consume only a portion of the input string
   (any unconsumed input should remain available to process later)

## Parse the first character

Say we want the first character of the input string.
Our parser should return `Char` (not `String`).
But we also want the unparsed part of the input, for further parsing.
Our parser should return that too, e.g. in a tuple wit the
result first and the unparsed string : `('H', "ello")`.
(You might prefer a record

If we convert the input string to list of characters,
we can use pattern matching to extract the head and build the result tuple:

[cons]: http://package.elm-lang.org/packages/elm-lang/core/5.1.1/List#::	"cons"

```elm
char0 : String -> (Char, String)
char0 input ->
 case String.toList input of
    (c::cs) -> (c, String.fromList cs)
    []      -> -- FAILURE!
```

When we give this parser an empty string, the second pattern matches.
Parsing has failed!

## Handling failure

The parser needs a way to report that it has no result to return, because parsing failed.
Returning a tuple of the result and the unparsed string will not suffice.

- [elm-tools/Parser][parser] uses a [Result][result], so it can return helpful messages about the failure
- [Hutton and Meijer][hutton] wrap results in a list, and use an empty list to represent failure

[hutton]: http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
[parser]: http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser
[result]: http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Result

These are the notes from a presentation that
I give by live-coding this parser library.
To keep typing to a minimum, I use `Maybe.Nothing`
to denote failed parsing, and wrap successful parser results with `Just`.
So in the case of failure we will return `Nothing`.

A more helpful solution might be to use [`Result`][result] and use
the `Err` constructor to return a helpful error message in he case of failure.

[result]: http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Result

**Note**: Strings aren't *natively* `List Char` in Elm,
so they don't pattern match with `(::)`. But we can convert them:

```elm
char0 : String -> Maybe (Char, String)
char0 input =
    case String.toList input of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, String.fromList tail )
```

The Elm standard libraries have a function that does just this: [uncons][uncons]:

[uncons]: http://package.elm-lang.org/packages/elm-lang/core/5.1.1/String#uncons

 ```elm
char0 : String -> Maybe (Char, String)
char0 =
    String.uncons
 ```

## A type for parsers

In genera we won't always want parsers that return `Char`.
We can generalise the type of `char0` with a type variable to get a type for parsers:

```elm
type alias Parser a
    = String -> Maybe (a, String)
```

And we can use it to give a type to `char0`:

```elm
char0 : Parser Char
char0 =
    String.uncons
```

## Running parsers

Lets try out our parser in the elm repl:

```elm
> character0 "abc"
  Just ('a',"bc") : Maybe ( Char, String )
```

`run` makes it a bit easier to run parsers from the command line
and strips out the returned value from the tuple. We need to keep
the Maybe, though, to represent failure.

```elm
run : String -> Parser a -> Maybe a
run s p =
    p s |> Maybe.map Tuple.first
```

Now we can _run_ `character0` on a given string:

```elm
> run "abc" character0
  Just 'a' : Maybe Char
```

## Pattern matching

What if we want to match only a specific character,
or character-class (digits, letters, etc.)?
For example, lets parse only digits (`0-9`).
Here's the plan:

```
Parse a character with `character0`
  and then...
      if that character was a digit,
        parsing succees with that character
      else
        parsing fails
```

The code will follow this plan closely,
and we begin by defining a function `andThen`
that allows us to compose parsers.

## Combining parsers

```elm
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen continuation parser =
    \input ->
        case parser input of
            Just ( aVal, rest ) ->
                continuation aVal rest

            Nothing ->
                Nothing
```

`andThen` is the name commonly given to functions that
act as Monadic *bind* (`>>=` in Haskell).
Its first two arguments are flipped
round so it can be used in a sort of infix way as part
pipelines constructed with [`(|>)`][ffa]:

[ffa]: http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Basics#|>

```elm
  parser
    |> andThen continuation
```

Because `Parser =  String -> Maybe (a, String)`
and `continuation: a -> String -> Maybe (b, String)`,
If we *uncurry* `continuation`, its type becomes:
`(a, String) -> Maybe (b, String)`
and we can reuse `Maybe.andThen`:

```elm
andThen_ : (a -> Parser b) -> Parser a -> Parser b
andThen_ continuation parser =
   parser >> Maybe.andThen (uncurry continuation)
```

Now we can use `andThen` to build our parser for digits:

```
     character0
        |> andThen
            (\c ->
                if Char.isDigit c then
                    -- parsing succeeds, and the result is c
                else
                    -- parsing fails
            )
```

Instead of building the succeed and fail cases with
tuples and Maybe, lets define some helper functions:

## Always succeed

A parser that never fails, and always returns a given
value `val`, together with the entire unconsumed input `input`:

```elm
succeed : a -> Parser a
succeed val =
    \input -> Just ( val, input )
```

## Always fail

Ignore the input and just fail:

```elm
fail : Parser a
fail =
    \input -> Nothing
```

## Digit parser

We complete the digit parser `digit` using `succeed` and `fail`:

```elm
digit : Parser Char
digit =
    character0
        |> andThen
            (\c ->
                if Char.isDigit c then
                    succeed c
                else
                    fail
            )
```

## Satisfies

Generalising:`Char.isDigit` becomes any predicate on `Char`:

```elm
satisfies : (Char -> Bool) -> Parser Char
satisfies predicate =
    character0
        |> andThen
            (\c ->
                if predicate c then
                    succeed c
                else
                    fail
            )
```

- Rewrite `digit`:

  ```elm
  digit : Parser Char
  digit =
      satisfies Char.isDigit
  ```

## Other Char parsers

Now we can match specific characters or character groups:

```elm
upper : Parser Char
upper =
    satisfies Char.isUpper

char : Char -> Parser Char
char c =
    satisfies (\x -> x == c)
```

## Convert to other types: intDigit

What if we want results of some other type than Char?
E.g.: parse a digit and then convert it to `Int`.

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
```
- Or, point free:
  ```elm
  intDigit : Parser Int
  intDigit =
      digit |> andThen (succeed << toInt)
  ```

##Map

Generalising:

- `digit` becomes any parser `p :  Parser a`
- `toInt` becomes any function `f : a -> b`

```elm
map : (a -> b) -> Parser a -> Parser b
map f p =
    p |> andThen (\a -> succeed (f a))
```

- Or, point free:

    ```elm
    map : (a -> b) -> Parser a -> Parser b
    map f =
        andThen (succeed << f)
    ```

- And we an rewrite `intDigit`using map:

    ```elm
    intDigit : Parser Int
    intDigit =
        digit |> map toInt
    ```

What if we want the multi-digit version of this?

## Take

`take` maps a function wrapped in a parser (parsers are applicative functors):

```elm
take : Parser a -> Parser (a -> b) -> Parser b
take pa pf =
    pf |> andThen (\f -> map f pa)
```

Take has its its arguments flipped for *infix* use in `|>` pipelines.

To use it we need a _function parser_ `pf : Parser (a -> b)`: a parser that returns a function:

- Use `succeed`. E.g. make a parser that returns  `toInt`:

  ```elm
  > succeed toInt
    <function> : Parser.Parser (Char -> Int)
  ```

- Now we can rewrite  `intDigit:

  ```elm
  intDigit1 : Parser Int
  intDigit1 =
      succeed toInt
          |> take digit
  ```

- Or use a pipe :

  ```elm
  > run (succeed List.singleton |> take digit ) "50"
  Just ['5'] : Maybe (List Char)
  ```

## Partial application

Take is more interesting with multi-argument, curried functions.

E.g.: `(+) : number -> number -> number`

- Partially applying this, with `take`, is another way to get a function parser:

  ```elm
  > succeed (+)
    <function> : Parser.Parser (number -> number -> number)

  > succeed (+) |> take intDigit
    <function> : Parser.Parser (Int -> Int)

  > succeed (+) |> take intDigit |> take intDigit
    <function> : Parser.Parser Int

  > run (succeed (+) |> take intDigit |> take intDigit) "12"
  Just 3 : Maybe.Maybe Int
  ```

- or with `(,)`:

  ```elm
  mkTuple = succeed (,)
      |> take digit
      |> take digit
  ```

  ```elm
  > run mkTuple "50"
    Just ('5','0') : Maybe ( Char, Char )
  ```

## Ignore stuff

`drop` is a parser that applies another parser (which must match) but ignores its output.

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

- It works like this:

  ```elm
  > (upper |> drop digit ) "A5"
    Just ('A',"") : Maybe.Maybe ( Char, String )
  ```

  Which seems odd, but wait!

Now we can chose what to `take`, and what to `drop`:

```elm
pair : Parser ( Int, Int )
pair =
    succeed (,)
        |> drop (char '(')
        |> take intDigit
        |> drop (char ',')
        |> take intDigit
        |> drop (char ')')
```
- Discard concrete syntax; keep concrete syntax:

  ```elm
  > run pairN "(4,3)"
    Just (4,3) : Maybe.Maybe ( Int, Int )
  ```


## Parser pipelines

`elm-tools/Parser` does this with its own infix [pipe operators](https://github.com/elm-tools/parser#parser-pipelines):

```elm
apply : (a -> b) -> a -> b
apply f a =
    f a

map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 f pa pb =
    pa |> andThen (\a -> pb |> map (\b -> f a b))

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
```

```elm
> run pair1 "(5,0)"
  Just (5,0) : Maybe ( Int, Int )
```

## Repetition

Suppose we want to take several digits, e.g. a list of characters:

- can use cons `(::)`

  ```elm
  > succeed (::)
    <function> : Parser.Parser (a -> List a -> List a)

  > succeed (::) |> take digit
    <function> : Parser.Parser (List Char -> List Char)
  ```


Now we need a parser `digits : Parser (List Char)`

  ​    `succeed (::) |> take digit |> take digits`

- We would write this recursively:
    **either** the input has some number of digits,
    ​    in which case we **take** the first and cons it with the rest
    **or** no digits -- our list is  empty

    ```elm
    {--
      digits : Parser (List Char)
      digits =
        either
            (succeed (::)
                |> take ditig
                |> take digits
            )
            (succeed [])
    --}
    ```

## Either

If the first parser succeeds, we're done, otherwise use the second one

```elm
either : Parser a -> Parser a -> Parser a
either p1 p2 =
    \input ->
        case p1 input of
            Just result ->
                Just result

            Nothing ->
                p2 input
```

- More consisely, using Maybe:

  ```elm
  either : Parser a -> Parser a -> Parser a
  either parser1 parser2 =
      \input ->
          parser1 input
          |> Maybe.map Just
          |> Maybe.withDefault (parser2 input)
  ```


## Many digits

```elm
digits : Parser (List Char)
digits =
    either
        (succeed (::)
            |> take ditig
            |> take digits -- Uh oh!
        )
        (succeed [])
```

`digits` is defined in terms of itself and (eager) Elm doesn't like it. Force it to be lazy: hide the recursion into an anonymous function, using `lazy`:

```elm
lazy : (() -> a -> b) -> (a -> b)
lazy f =
    \a -> f () a
```

```elm
digits : Parser (List Char)
digits =
    either
        (succeed (::)
            |> take digit
            |> take (lazy (\() -> digits))
        )
        (succeed [])
```

- now we can parse as many digits as we can find:

  ```elm
  > run digits "123abc"
    Just ['1','2','3'] : Maybe.Maybe (List Char)
  ```


## Many

Generalising:

- `digit` becomes any parser `p : Parser a`

```elm
many : Parser a -> Parser (List a)
many parser =
    either
        (succeed (::)
            |> take parser
            |> take (lazy (\() -> many parser))
        )
        (succeed [])
```

- Now we can rewrite `digits`:

  ```elm
  digits0 : Parser (List Char)
  digits0 =
      many digit
  ```

- and run it:

  ```elm
  > run digits "123abc"
    Just ['1','2','3'] : Maybe.Maybe (List Char)
  ```



## Mutli-digit integers

`many intDigit : Parser.Parser (List Int)` so we just need a way to convert `List Int` to `Int`

```elm
intFromList : List Int -> Int
intFromList =
    List.foldl (\i a -> a * 10 + i) 0
```

```elm
integer : Parser Int
integer =
    many intDigit
        |> map intFromList
```

## Detectin    g the end of input

```elm
end : Parser ()
end =
    \input ->
        if String.isEmpty input then
            Just ( (), input )
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



