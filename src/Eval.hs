module Eval where

import Value
import Parser

eval :: Value -> Value

-- things that eval to themselves: all atoms and quoted things
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val

-- unquoted lists are function applications
--  and eval to the result of the application
eval (List (Atom func : args)) = apply func $ map eval args
apply :: String -> [Value] -> Value
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

car :: Value -> Value
car (List (x:xs)) = x
cdr :: Value -> Value
cdr (List (_:xs)) = List xs
listPrimitives = [("car", unaryOp car),
                  ("cdr", unaryOp cdr)]

numberPrimitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

symbolPredicate = (===) (Atom "")
symbol2string (Atom str) = String str
symbolPrimitives :: [(String, [Value] -> Value)]
symbolPrimitives = [("symbol?", unaryOp symbolPredicate),
                    ("symbol->string", unaryOp symbol2string)]

primitives = numberPrimitives ++
             symbolPrimitives ++
             listPrimitives

unaryOp :: (Value -> Value) -> [Value] -> Value
unaryOp op [x] = op x
--unaryOp op xs = TODO error or something

numericBinop :: (Integer -> Integer -> Integer) -> [Value] -> Value
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Value -> Integer
unpackNum (Number n) = n
