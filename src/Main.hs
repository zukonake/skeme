module Main where
import System.IO
import Parser
import Eval

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == ":quit" || input == ":q"
        then putStrLn "Quitting..."
        else do
            putStrLn $ show $ eval $ readExpr input
            main
