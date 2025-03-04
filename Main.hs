module Main where

import System.Environment (getArgs)
import Interpret

main :: IO ()
main = do
    args <- getArgs
    case args of
        (fileName : _) -> do
            contents <- readFile fileName
            let (stack, output) = interpret contents
            putStrLn output
            if null stack
                then putStrLn "Execution finished with an empty stack."
                else putStrLn ("Stack content: " ++ show stack)
        _ -> putStrLn "Error: No input file provided."
