module Eval where

import Value
import Parser

eval :: Value -> Value
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val

eval (List (Atom func : args)) = apply func $ map eval args
apply :: String -> [Value] -> Value
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [Value] -> Value)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [Value] -> Value
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Value -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
