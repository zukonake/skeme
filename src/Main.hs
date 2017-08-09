module Main where
import System.IO
import Parser

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == ":quit" || input == ":q"
        then putStrLn "Quitting..."
        else do 
            putStrLn $ readExpr input
            main
