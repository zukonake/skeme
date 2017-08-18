module Eval (eval) where

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
--eval _ = TODO error

apply :: String -> [Value] -> Value
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

car :: Value -> Value
--car _ = TODO error
car (List (x:_)) = x

cdr :: Value -> Value
--cdr _ = TODO error
cdr (List (_:xs)) = List xs

type Primitive = (String, [Value] -> Value)

listPrimitives :: [Primitive]
listPrimitives = [("car", unaryOp car),
                  ("cdr", unaryOp cdr)]

numberPrimitives :: [Primitive]
numberPrimitives = [("+", numericBinOp (+)),
                    ("-", numericBinOp (-)),
                    ("*", numericBinOp (*)),
                    ("/", numericBinOp div),
                    ("mod", numericBinOp mod),
                    ("quotient", numericBinOp quot),
                    ("remainder", numericBinOp rem)]

symbolToString :: Value -> Value
symbolToString (Atom str) = String str
--symbolToString _ = TODO error

typePrimitives :: [Primitive]
typePrimitives = [("symbol?", unaryOp $ (===) Atom {}),
                  ("list?", unaryOp $ (===) List {}),
                  ("number?", unaryOp $ (===) Number {}),
                  ("string?", unaryOp $ (===) String {}),
                  ("character?", unaryOp $ (===) Character {}),
                  ("bool?", unaryOp $ (===) Bool {}),
                  ("symbol->string", unaryOp symbolToString)]

primitives :: [Primitive]
primitives = numberPrimitives ++
             typePrimitives ++
             listPrimitives

unaryOp :: (Value -> Value) -> [Value] -> Value
unaryOp op [x] = op x
--unaryOp op _ = TODO error or something

numericBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Value
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Value -> Integer
unpackNum (Number n) = n
--unpackNum _ = TODO error
