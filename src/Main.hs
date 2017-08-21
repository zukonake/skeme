module Main where

import System.IO
import Parser
import Eval
import Control.Monad
import LangError

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == ":quit" || input == ":q"
        then putStrLn "Quitting..."
        else do
            evaled <- return $ liftM show $ readExpr input >>= eval
            putStrLn $ extractValue $ trapError evaled
            main
