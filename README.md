# Implementing an interpreter for Scheme in Haskell

Notes I made while learning how to write a Scheme interpreter, using the book 

https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours


# How to run the interpreter
```
ghci -package mtl-2.2.2   
```

then in Prelude
```
ghci >:l Main.hs
ghci > repl
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

( Note that ```numericBinop (+)``` is the partial function, not ```numericBinop```)

* Now, we will add ```numBoolBinop```, ```boolBoolBinop```, and ```strBoolBinop```.

These new helper functions only take two arguments (each of different types), and return a ```bool```


* We create a ```boolBinop``` to abstract out the common logic between them, as they differ only in the type of arguments.

* Note that, ```unpacker``` is a generic function that takes a LispVal and turns it into a ```ThrowsError a```, the ```a``` here can be any haskell type.

You see, we have represented the orginal input as a ```LispVal```, which is very neat. It is a type that exactly represents the semantics behind a Lisp expression (represented in Haskell types).

Now, using the ```unpacker``` we are removing the ```Lispy-ness``` and converting the object back into some haskell type for evaluation.

This is important, because ```apply``` indirectly calls ```unpacker```.

## Here is the call sequence/summary so far

1. ```main``` reads the input string using ```readExpr2```
2. ```readExpr2``` parses the string, and converts it into some type of ```LispVal```, remember that ```LispVal``` is an ```OR```  type (i.e. it is composed of many types). ```readExpr2``` does the "classification" as well.
3. this ```LispVal``` is then passed to ```eval``` for evaluation. 
4. when we give ```eval``` a LispVal representing a function application. It evaluates the operands first, and then passes the operands and the operators to ```apply```

Note that ```eval``` calls ```apply``` only for one pattern -- when we give it a "lisp function application". i.e. give it a function to evaluate.

5. ```apply``` looks up the given operator in our map ```primitives```
6. ```primitives``` returns a partially applied function based on the operator. Example ```numericBinop (+)``` 
7. now, apply uses this partial function obtained in step 6, and "gives" it the set of operands (i.e. arguments)
8. Note that the operands that ```apply``` got, they had come from ```eval```, and hence they are of the type ```LispVal```(eval only returns an Either ParseError LispVal)

So now we use the ```unpacker``` (or ```unpackNum```) to convert them, i.e. to bring them out of the context. And convert them into a plain old haskell type.
9. these functions ```numericBinop```, etc do the evaluation.
10. ```boolBinop``` is particularly important, it takes in an arbitrary unpacker (of Lisp strings/Nums/Bool), it takes an operator, it takes in the params.

```
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
```
11. We use it to create specfic evaluation functions, like 
```numBoolBinop = boolBinop unpackNum```
so the partially applied ```numBoolBinop``` will be expecting an operator and a ```[LispVal]``` to evaluate the given boolean (lisp) expression at hand.

Note that it is ```primitives``` itself which returns to ```apply```, a partial application of (say) ```numBoolBinop```.
Example, it might return you ```numBoolBinop (<)```

To which apply will further send the operands to.

* Note how cool is that how much partial applications are being used here.

```apply``` whose job is to (literally) apply the operator with the operand, gets the operator as a 'String'.

It looks the operator up in the ```primitives```, 
```primitives``` sees what kind of operator is that, ```primitives``` *knows* the appropritate binop (Binary Operator) method associated with each of these operators, like "+" gets the ```numericBinop (+)```.

And again a partial function is returned. 

```numericBinop``` (and its family) are *the* evaluation functions here. They take the operator. They take the params. (They are aware of which unpacker to use for which kind of params). And do the groundwork for evaluating our expressions. 

# Pattern Matching : Conditionals 

 As with standard Scheme, our evaluator considers #f to be false and any other value to be true.

## if / else

 This can be added simply by adding an eval clause. The expressions which are inside the if statements (like the conditional), and the two alternatives will use other eval patterns that we have already implemented quite trivially.
 ```
eval (List [Atom "if", pred, conseq, alt ] ) = do
                                               -- evaluate the predicate
                                               result <- eval pred  
                                               case result of 
                                                    Bool False -> eval alt
                                                    otherwise  -> eval conseq
```                                                    

# Implementing List Primitives


## Implementing ```car```, ```cdr``` and ```cons```

Examples of ```car``` in Scheme

```
(car '(a b c)) = a
(car '(a)) = a
(car '(a b . c)) = a
(car 'a) = error – not a list
(car 'a 'b) = error – car only takes one argument
````


Examples for ```cdr``` in Scheme,
```
(cdr '(a b c)) = (b c)
(cdr '(a b)) = (b)
(cdr '(a)) = NIL
(cdr '(a . b)) = b
(cdr '(a b . c)) = (b . c)
(cdr 'a) = error – not a list
(cdr 'a 'b) = error – too many arguments
```

We will directly implement these functions in our interpreter, as they're quite trivial to implement.

We define a function ```car``` which takes a ```[LispVal]``` and returns ```ThrowsError LispVal```


(read the List Primitives section here,
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_2#List_Primitives:_car,_cdr,_and_cons)


We implement these list operations. And then add them to our function dictionary i.e. ```primitives```, so that they can be used by the evaluation function ```apply```.



## Weak Typing


We introduce a function ```equal?``` which checks for equality regardless of the type of the value.

Example, we have ```eqv```
```
(eqv? 2 "2") = #f
```
We want to implement ```equal?```, such that
```
(equal? 2 "2") = #t
```
To do this we need to use a GHC extension i.e. *Existential Types*,
which lets us create a heterogenous list.


### **How do we implmenet ```equal?```**

From the book,
*We try all our unpack functions, and if any of them return Haskell values that are equal, we return True.

We store all the unpacking functions in a list, and use mapM to execute them in turn*


So, we define a **data type** that can hold any function of the form ```LispVal -> something```, where the type ```something``` s is of the type ```Eq``` (that is, supports equality).





# Progress Checklist


### todo:

- adding list primitives
- building a proper repl
- adding variables and assignments
- adding support for definings functions
- refactor codebase to use stack, and more efficient imports

done:

- converted readExpr and Eval to use proper error handling
- modify primitives, and numericBinop to do the same 
- after error handling is complete, go through the entire codebase, refactor it for readability, add docs and explanations
- to add conditionals, other logical operations.

