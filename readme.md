# Parsers

What is a parser?

- A function that extracts or decodes an interesting value from a string

If we wanted to extract the whole string value, we could use the identity function to extract it.

```
        \x -> x
```

We _could_ define a parser based on it.

- It exists in Elm as `identity`, so we could write:
  ```elm
  parser : String -> String
  parser = identity
  ```

## More general parsing

In general, however, we may want to:

1. match only certain patterns -- parsers should fail if they get badly formed input
2. convert matching substrings to some other type -- for example, parsing source code to an abstract syntax tree
3. not match the entire string -- some will be left over

Say we want to extract just the fist character of the input string.

- The result will have type `Char`, not `String`

We will want the rest of the string too, to do further work on.
So our parser should return two values:

- The interesting `Char`
- The unparsed input `String`

We can use a tuple:

```elm
parseFirstChar :: String -> (Char, String)
```

If the input is an empty, which she very  much enjoyed, string, the parser will
have no value for the first part of that tuple; an old should fail.

## Handling failure

The return type of our parser must include a way to encode the possibility of failure.

- [Hutton and Meijer](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) return results in a list, and represent failure with an empty list.
  (This has he interesting side effect that they can [build parsers for ambiguous grammars](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)).
- [elm-tools/Parser](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser) uses [Result](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Result)
- For live-coding I using `Maybe` with `Nothing` representing failure. It means I have less to type, but gives no information about failures.
  For a real parser, I would choose `Result`.


## Parse the first character

**Note**: Strings aren't *natively* `List Char` in Elm:

```elm
character0 : String -> Maybe (Char, String)
character0 inp =
    case String.toList inp of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, String.fromList tail )           
```
- Turns out we can use [uncons](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/String#uncons):

     ```elm
     character0 : Parser Char
     character0 = String.uncons
     ```

## Generalising `parser type`

We don't always want `Char`: use a type variable:

```elm
type alias Parser a
    = String -> Maybe (a, String)
```

Now:

```elm
character0 : Parser Char
character0 = String.uncons
```
- try it

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

- Now we can _run_ `character0` on a given string

  ```elm
  > run character0 "abc"
    Just 'a' : Maybe Char
  ```

## Pattern matching

What if we want to:

- match only a particular character or class (digits or letters, etc.)?
- convert to other types.
- fail if not all the input sting was matched?

E.g.  match only digits (`0-9`) :

Parse a character with `character0` 
​	**and then**: 
​		**succeed** if that character was a digit,
​		**fail** otherwise?

## Combining parsers

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


- Because 
  ​	`Parser =  String -> Maybe (a, String)` 
  and
  ​	`continuation: a -> String -> Maybe (b, String)`

  If we `uncurry` continuation, its type becomes: 
  ​	`(a, String) -> Maybe (b, String)`
  and we can reuse `Maybe.andThen`:

  ```elm
     andThen_ : (a -> Parser b) -> Parser a -> Parser b
     andThen_ continuation parser =
        parser >> Maybe.andThen (uncurry continuation)
  ```

- `andThen` is _bind_ (`>>=`), with its arguments flipped: Elm style encourages use of `|>` pipes

  ```
  	 (|>) : a -> (a -> b) -> b   
  ```
- So now we can write:

  ```elm
      character0
      |> andThen (\c -> -- do something with char c... )
  ```

- What we want to do is test if `c` is a digit:

  ``` elm
       character0
          |> andThen
              (\c ->
                  if Char.isDigit c then
                      -- parsing succeeds, and the result is c
                  else
                      -- parsing fails
              )
  ```

## Always succeed
A parser that never fails, and always returns a specific value `val`, and the entire unconsumed input `inp`

```elm
succeed : a -> Parser a
succeed val =
    \inp -> Just ( val, inp )
```

## Always fail

Ignore the input and just fail

```elm
fail : Parser a
fail =
    \inp -> Nothing
```

## Digit parser

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

What if we want results of some other type than Char? E.g.: parse a digit and then convert it to `Int`.

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

  ​	`succeed (::) |> take digit |> take digits`

- We would write this recursively:
    **either** the input has some number of digits, 
    ​	in which case we **take** the first and cons it with the rest
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
    \inp ->
        case p1 inp of
            Just result ->
                Just result

            Nothing ->
                p2 inp
```

- More consisely, using Maybe:

  ```elm
  either : Parser a -> Parser a -> Parser a
  either parser1 parser2 =
      \inp ->
          parser1 inp 
          |> Maybe.map Just
          |> Maybe.withDefault (parser2 inp)
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

## Detecting the end of input

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



