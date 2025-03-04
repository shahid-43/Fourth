module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char (chr)
import Debug.Trace  -- For debugging

type Stack = [Val]

-- Removes all extra double quotes
stripQuotes :: String -> String
stripQuotes s = filter (`notElem` "\"") s  

-- Main evaluation function for operators
eval :: String -> Stack -> Either String Stack
-- Multiplication (supports both Integer and Float)
eval "*" (Integer x : Integer y : tl) = Right (Integer (x * y) : tl)
eval "*" (x : y : tl) = Right (Real (toFloat x * toFloat y) : tl)
eval "*" _ = Left "Stack underflow"

-- Addition, Subtraction, Division, Power
eval "+" (Integer x : Integer y : tl) = Right (Integer (y + x) : tl)
eval "-" (Integer x : Integer y : tl) = Right (Integer (y - x) : tl)
eval "/" (Integer x : Integer y : tl)
  | x == 0    = Left "Division by zero error"
  | otherwise = Right (Integer (y `div` x) : tl)
eval "^" (Integer x : Integer y : tl) = Right (Integer (y ^ x) : tl)

-- DUP (Duplicate the top element)
eval "DUP" (x : tl) = Right (x : x : tl)
eval "DUP" [] = Left "Stack underflow"

-- EMIT (Push character as string instead of printing immediately)
eval "EMIT" (Integer x : xs) = Right (Id [chr x] : xs)
eval "EMIT" _ = Left "Stack underflow"

-- CR (Push newline as string instead of printing immediately)
eval "CR" xs = Right (Id "\n" : xs)

-- STR (Convert to string, ensuring no extra quotes)
eval "STR" (Id x : xs) = Right (Id (stripQuotes x) : xs)
eval "STR" (Integer i : xs) = Right (Id (show i) : xs)
eval "STR" (Real x : xs) = Right (Id (show x) : xs)

-- CONCAT2 and CONCAT3 (String concatenation, removing extra quotes)
eval "CONCAT2" (Id s1 : Id s2 : xs) = Right (Id (stripQuotes s2 ++ stripQuotes s1) : xs)
eval "CONCAT3" (Id s1 : Id s2 : Id s3 : xs) = Right (Id (stripQuotes s3 ++ stripQuotes s2 ++ stripQuotes s1) : xs)

-- Default case: Preserve as a string argument
eval s l = Right (Id s : l)

-- variant of eval with output (used for `.` command)
evalOut :: String -> (Stack, String) -> Either String (Stack, String)
evalOut "." (Id x : tl, out) = Right (tl, out ++ stripQuotes x)
-- evalOut "." (Id x : tl, out) = trace ("DEBUG: Printing " ++ x) (Right (tl, out ++ stripQuotes x))
evalOut "." (Integer i : tl, out) = Right (tl, out ++ show i)
evalOut "." (Real x : tl, out) = Right (tl, out ++ show x)
evalOut "." ([], _) = Left "Stack underflow"
evalOut "EMIT" (Id c : tl, out) = Right (tl, out ++ c)
evalOut "CR" (Id c : tl, out) = Right (tl, out ++ c)
evalOut op (stack, out) = case eval op stack of
    Right newStack -> Right (newStack, out)
    Left err -> Left err
