{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
module Parsers where

import Control.Monad
import Control.Applicative
import Data.Char (isDigit, digitToInt, isSpace)

----------
--TASK 1--
----------
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

--Exercise 1
-- | Parser which parse input and apply func to result of parsing
instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    --f a = b - we parse it, and get Maybe (a, [s]), then we fmap it and get Maybe (b, [s])
    fmap f (Parser innerF) = Parser (fmap (\(a, st) -> (f a, st)) . innerF)

--Exercise 2
-- | Parser which first runs first parser and then parse the remaining with second parser
-- | The result of parsing is applying function from first parsing result to second result
instance Applicative (Parser s) where
    pure :: a -> Parser s a
    --pure a represents the parser which consumes no input
    --and successfully returns a result of a
    pure elem = Parser (\s -> return (elem, s))
    (<*>) :: Parser s (a1 -> a2) -> Parser s a1 -> Parser s a2
    --p1 <*> p2 represents the parser which first runs p1 (which will
    --consume some input and produce a function), then passes the
    --remaining input to p2 (which consumes more input and produces
    --some value), then returns the result of applying the function to the
    --value. However, if either p1 or p2 fails then the whole thing should
    --also fail (put another way, p1 <*> p2 only succeeds if both p1 and
    --p2 succeed).
    --m a -> (a -> m b) -> m b
    --(a -> m b) -> (b -> m c) -> a -> m c
    --use fish to compose result
    Parser p1f <*> Parser p2elem = Parser (p1f >=> (\(func, s1) -> --first parse done, we get function to apply and input s1
                                                     p2elem s1 >>= \(a, s2) -> Just (func a, s2)))
                                                     --parse new input (s1) and now apply func to result on second parse

--Exercise 4
-- | Parse input with 1st parser, if it fails parse it with 2nd parser
instance Alternative (Parser s) where
   --(<|>) is intended to represent choice: that is, f1 <|> f2 represents
   --a choice between f1 and f2. empty should be the identity element for
   --(<|>), and often represents failure.
   --Write an Alternative instance for Parser:
   -- empty represents the parser which always fails.
   -- p1 <|> p2 represents the parser which first tries running p1. If
   --p1 succeeds then p2 is ignored and the result of p1 is returned.
   --Otherwise, if p1 fails, then p2 is tried instead.
   empty :: Parser s a
   empty = Parser $ const Nothing
   (<|>) :: Parser s a -> Parser s a -> Parser s a
   Parser p1 <|> Parser p2 = Parser (\input -> case p1 input of
                                                 Nothing -> p2 input
                                                 something -> something)

-- | Monad instance for parser, we will get another parser after binding
instance Monad (Parser s) where
    (>>=) :: Parser s a1 -> (a1 -> Parser s a2) -> Parser s a2
    Parser p >>= f = Parser (p >=> (\(a, input) -> runParser (f a) input))

----------
--TASK 2--
----------

--From introduction

-- | satisfy takes a Char predicate and constructs a
-- parser which succeeds only if it sees a Char that satisfies
-- the predicate (which it then returns). If it encounters a Char that does not
-- satisfy the predicate (or an empty input), it fails
satisfy :: (s -> Bool) -> Parser s s
satisfy pred = Parser func
    where
     func [] = Nothing
     func (x : xs)
           | pred x = Just (x, xs)
           | otherwise = Nothing

-- | define the parser char, which expects to
--see exactly a given character and fails otherwise.
element :: Eq s => s -> Parser s s
element symb = satisfy (== symb)


--if the lst is empty - then just return
--else we need to parse x and xs separately
--parse x with 'element', get (a, input1)
--after that parse input1 recursively, concate answer
-- | Do the same as element but with [s]
stream :: Eq s => [s] -> Parser s [s]
stream lst = case lst of
                   (x : xs) -> Parser (\input -> do
                                        (a1, input1) <- runParser (element x) input
                                        (a2, input2) <- runParser (stream xs) input1
                                        Just (a1 : a2, input2)
                                       )
                   [] -> Parser (\input -> Just ([], input))

-- | Parser which doesn`t fail at all and don`t consume input
ok :: Parser s ()
ok = return ()

-- | Check if we finish with stream or fail otherwise
eof = Parser (\input -> case input of
                               [] -> Just ((), [])
                               _ -> Nothing
                               )

----------
--TASK 3--
----------

-- | Correct bracket sequence = e | (S) | S1S2
data CBS
   = Empty
   | Inner CBS
   | Concate CBS CBS

-- | Parse if the input is CBS or fails otherwise
cbsParser :: Parser Char CBS
cbsParser = findCBS <* checkBalance
  where
    --just empty input
    emptyCase = Empty <$ ok

    --if not empty, than we can make like (S) + other
    cbsCase = (Concate <$> (Inner <$> (element '(' *> findCBS <* element ')'))) <*> findCBS

    --it`s or cbs or some empty input
    findCBS = cbsCase <|> emptyCase

    --reuse for better logical understanding
    checkBalance = eof

instance Show CBS where
   show Empty = ""
   show (Inner cbs) = "(" ++ (show cbs ++ ")")
   show (Concate cbs1 cbs2) = show cbs1 ++ show cbs2

--by default number has no sign
-- | Parse signed or unsigned number in input
numbParser :: Num t => Parser Char t
numbParser = fmap snd nonsignCase <|> signCase
   where
      nonsignCase :: Num t => Parser Char (t, t)
      nonsignCase = do
        dig <- satisfy isDigit
        (place, add) <- nonsignCase <|> stop
        let curAnswer = myDigitToNum dig * place + add
        let curPlace = place * 10
        return (curPlace, curAnswer)
      --neutral element returned in stop
      stop :: Num t => Parser Char (t, t)
      stop = (1, 0) <$ ok
      signCase :: Num t => Parser Char t
      signCase = do
        sign <- satisfy (\symb -> (symb == '-') || (symb == '+'))
        unsignParsed <- fmap snd nonsignCase
        return (
                case sign of
                   '+' -> unsignParsed
                   '-' -> (-1) * unsignParsed
                )

-- | My impl of digitToNum polymorphic
myDigitToNum :: Num t => Char -> t
myDigitToNum '0' = 0
myDigitToNum '1' = 1
myDigitToNum '2' = 2
myDigitToNum '3' = 3
myDigitToNum '4' = 4
myDigitToNum '5' = 5
myDigitToNum '6' = 6
myDigitToNum '7' = 7
myDigitToNum '8' = 8
myDigitToNum '9' = 9

--just parse first number and then recursively parse other numbs
--with depth of recur defined as first number
-- | Parse one list of integer numbers
listParser :: (Ord t, Num t) => Parser Char [t]
listParser = do
  whitespaces
  leng <- numbParser
  if leng >= 0
    then parseCur leng
    else empty
  where
    parseCur :: (Ord t, Num t) => t -> Parser Char [t]
    parseCur curLeng
      | curLeng > 0 = do
        commas
        curNumb <- numbParser
        otherNumbs <- parseCur (curLeng - 1)
        pure (curNumb : otherNumbs)
      | otherwise = return []

whitespaces = many (satisfy isSpace)

commas = whitespaces *> satisfy (== ',') <* whitespaces

--parse first list, then parse others and concate them
-- | Parse two decimal array of integer numbers
listlistParser :: (Ord t, Num t) => Parser Char [[t]]
listlistParser = do
   whitespaces
   parseNext <|> emptyCase
   where
      emptyCase = [] <$ eof
      parseNext :: (Ord t, Num t) => Parser Char [[t]]
      parseNext = do
           curList <- listParser
           otherLists <- commas *> parseNext <|> emptyCase
           return (curList : otherLists)