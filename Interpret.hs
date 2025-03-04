module Interpret where
-- This file contains the FORTH interpreter

import Val
import Eval
import Flow

-- Inner function for foldl
-- Takes the current stack and an input and computes the next stack
evalF :: ([Val], String) -> Val -> ([Val], String)
evalF s (Id op) =
    case evalOut op s of
        Right newState -> newState  -- Extract `(Stack, String)`
        Left err -> (fst s, "Error: " ++ err)  -- Preserve stack, show error message

-- Cannot run, put on the stack and preserve output
evalF (s, out) x = (x : s, out)

-- Function to interpret a string into a stack and an output string
interpret :: String -> ([Val], String)
interpret text = text |>
    words |>      -- Break text into words
    map strToVal |>  -- Convert strings to instructions
    foldl evalF ([], "")  -- Perform evaluation
