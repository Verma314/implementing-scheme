# Implementing an interpreter for Scheme in Haskell

Notes I made while learning how to write a Scheme interpreter, using the book 

https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours


# How to run the interpreter
```
ghci -package mtl-2.2.2   
```

then in Prelude
```
>:l Main.hs
>ghciMain
```


# Parser

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

The key function that parses our strings, classifies them implicity, and returns a ``` Parser LispVal```

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



### Reading the input

This function literally converts the input into a Haskell type.

This uses the ```parseExpr``` to parse the input, 
the ```parse``` function returns an ```Either String (Parser LispVal)```.

```
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val ->  val 
```



# Evaluation

"The purpose of an evaluator is to map some "code" data type into some "data" data type, the result of the evaluation.

In Lisp, the data types for both code and data are the same, so our evaluator will return a LispVal

Evaluating numbers, strings, booleans, and quoted lists is fairly simple: return the datum itself."


**Status of the interpreter so far**

Parser's readExpr now can read input and return a LispVal, instead of String. Eval can take these LispVals and evaluate them (KINDOF). Main.hs is now used to call the parsing method (readExpr) and the evaluation method (eval)


## Adding basic primitives

This shall help us evaluate LispVals, and then return a LispVal.

```
eval :: LispVal -> LispVal
eval val@(Atom _) = val -- this is not correct
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val --  The last clause is our first introduction to nested patterns. The type of data contained by List is [LispVal], a list of LispVals. We match that against the specific two-element list [Atom "quote", val], a list where the first element is the symbol "quote" and the second element can be anything. Then we return that second element.
-- this belwo is for function evaluation, like (+ 1 2), which is LispVal terms is written like - [Atom "+",Number 1, Number 3]
-- we first evaluate the args too.
-- and then apply the operator to the result
eval (List ((Atom operator) : args)) =  apply operator $ map eval args
```

Read Eval.hs for more info. The code is well commented.

# Error Checking and Exceptions

(self explnatory. read comments in ErrorCatcher.hs to figure out Error handling)


# Partial Application, Conditionals, List Operations

* We'll add boolean operators, conditionals, and some basic string operations as built-in operations. We add these in the ```primitives``` function.

* Initially ```primitives``` would return just the ```numericBinop``` function partially applied.

Example given input "+" to ```primitives```,
it returns us ```numericBinop (+)```, which is a partially applied function that takes a list of lispVals and adds them all up.

Now, we will add ```numBoolBinop```, ```boolBoolBinop```, and ```strBoolBinop```.

These new helper functions only take two arguments (each of different types), and return a ```bool```


We create a ```boolBinop``` to abstract out the common logic between them, as they differ only in the type of arguments.

```unpacker``` is a generic function that takes a LispVal and turns it into a ```ThrowsError a```, the ```a``` here can be any haskell type.

You see, we have represented the orginal input as a ```LispVal```, which is very neat. It is a type that exactly represents the semantics behind a Lisp expression (represented in Haskell types).

Now, using the ```unpacker``` we are removing the ```Lispy-ness``` and converting the object back into some haskell type for evaluation.

This is important, because ```apply``` indirectly calls ```unpacker```.

### Here is the call sequence/summary so far

1. ```main``` reads the input string using ```readExpr2```
2. ```readExpr2``` parses the string, and converts it into some type of ```LispVal```, remember that ```LispVal``` is an ```OR```  type (i.e. it is composed of many types). ```readExpr2``` does the "classification" as well.
3. this ```LispVal``` is then passed to ```eval``` for evaluation. 
4. when we give ```eval``` a function application. It evaluates the operands first, and then passes the operands and the operators to ```apply```
5. ```apply``` loooks up the given operator in our map ```primitives```
6. ```primitives``` returns a partially applied function based on the operator. Example ```numericBinop (+)``` 
7. now, apply uses this partial function obtained in step 6, and "gives" it the set of operands (i.e. arguments)
8. Note that the operands taht ```apply``` got, they had come from ```eval```, and hence they are of the type ```LispEval```(eval only returns an Either ParseError LispVal)
9. So now we use the ```unpacker``` (or ```unpackNum```) to convert them, i.e. to bring them out of the original context. And convert them into a plain old haskell type.
10. these functions ```numericBinop```, etc do the evaluation.


# Progress Checklist

done:

- converted readExpr and Eval to use proper error handling
- modify primitives, and numericBinop to do the same 

todo:


- after error handling is complete, go through the entire codebase, refactor it for readability, add docs and explanations