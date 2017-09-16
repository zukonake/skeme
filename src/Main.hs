module Main where

import System.IO
import Parser
import Eval
import LangError

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == ":quit" || input == ":q"
        then putStrLn "Quitting..."
        else do
            let evaled = fmap show $ readExpr input >>= eval
            putStrLn $ extractValue $ trapError evaled
            main
