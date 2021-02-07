# implementing-scheme


## Parser

- We shall only deal with integers, symbols, functions, and list.

- Haskell comes standard with a parsing library called ```Parsec``` which implements a domain specific language for parsing text.

- To learn about the parsec library see,
http://book.realworldhaskell.org/read/using-parsec.html

- Parsec has a lot of built in parsers like ```string```, ```char```, ```digit```, ```letter``` etc.

- Our initial parser, just accepts these symbols,
```
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
```

We shall keep improving the parser.

- Our parser doesn't work with spaces. 

So we pass the Parser action ```space``` to the Parser action ```skipMany1```, to get a Parser that will recognize one or more spaces.

```
spaces :: Parser ()
spaces = skipMany1 space
```

The parser we end up with is,
```
parse (spaces >> symbol) "lisp" input
```

From the book

**...bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations.**

Also,

**..  Note that since we defined spaces in terms of skipMany1, it will no longer recognize a plain old single character. Instead you have to precede a symbol with some whitespace. We'll see how this is useful shortly**


## Using the Parser to create a data structure

"Right now, the parser doesn't do much of anything—it just tells us whether a given string can be recognized or not. Generally, we want something more out of our parsers: we want them to convert the input into a data structure that we can traverse easily. In this section, we learn how to define a data type, and how to modify our parser so that it returns this data type."   


We don't want the parser just to "accept/not-accept" an input, 
we want it to convert to proper data type values that haskell can understand.

```
-- how we parse a String into a LispVal
parseString :: Parser LispVal -- LispVal in a Parser Context
parseString = do 
              char '"'
              x <- many (noneOf "\"") -- can be anything except ```"```
              char '"'
              return $ String x
-- in the last line ^ We  apply the built-in function return to lift our LispVal into the Parser monad.                 
```

## Parsing Scheme Variables
First character should be a letter or symbol, 
followed by many letters or symbols or digits.

```
parseAtom = do 
              first <- letter <|> symbol 
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
```


## Parser for Numbers

We need to use ```liftM``` here,it is a monadic function, kinda similar to fmap.
```
*Main Control.Monad> :t liftM
liftM :: Monad m => (a1 -> r) -> m a1 -> m r

> :t (fmap)
(fmap) :: Functor f => (a -> b) -> f a -> f b
```

liftM can go inside the value in the monad context and apply the function.

To parse a number, we create a parser for digits ```many1 digit```. (many1 means it can parse one or more elements)

```many1 digit``` returns a string in a Parser context, i.e. ```Parse String```

We need to convert the string to a number, and then put this in our LispVal datastructure.

```
liftM (Number .read ) $ many1 digit
```


## Combining these "mini-parsers" 

We create a parser that can accept either a string, a number, or an atom,
```
parseExpr :: Parser LispVal
parseExpr = parseString <|> parseNumber <|> parseAtom
```

Examples,
```
*Main Control.Monad> parse parseExpr "" "HI"
Right (Atom "HI")
*Main Control.Monad> parse parseExpr "" "134"
Right (Number 134)
*Main Control.Monad> parse parseExpr "" "my name is aditya"
Right (Atom "my")
*Main Control.Monad> parse parseExpr "" "\"my name is aditya\""
Right (String "my name is aditya")
```

So our parser is able to identify what type of input we had, and able to create a ```Parser LispVal```, from raw input strings.

## Recursive Parsers: Adding lists, dotted lists, and quoted datums


### Lists

lists look like ```a b c d``` etc

we use ```sepBy``` to parse them. 
```
sepBy parseExpr spaces
```

But we gotta put it into the LispVal algebraic type,
so we do
```
parseList :: Parse LispVal
parseList =  liftM List $ sepByparseExpr spaces
```

### Dotted lists

```
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
```


### Quoted values
```
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
```

### Putting all our mini-parsers together
```
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
```                


