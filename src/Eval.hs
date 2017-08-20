module Eval (eval) where

import Value
import LangError

eval :: Value -> ThrowsError Value

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
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Value] -> ThrowsError Value
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args) $ lookup func primitives

car :: Value -> ThrowsError Value
car (List (x:_)) = x
car = notOfType "List" 

cdr :: Value -> ThrowsError Value
cdr (List (_:xs)) = List xs
cdr = notOfType "List"

type Primitive = (String, [Value] -> ThrowsError Value)

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

symbolToString :: Value -> ThrowsError Value
symbolToString (Atom str) = String str
symbolToString notSym = throwError $ TypeMismatch "symbol" (show $ toConstr notSym)


notOfType typeName val = throwError $ TypeMismatch typeName (show $ toConstr val)

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

unaryOp :: (Value -> Value) -> [Value] -> ThrowsError Value
unaryOp op [x] = return $ op x
unaryOp op list = throwError $ NumArgs 1 (length list)

numericBinOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value 
numericBinOp op params = return $ Number $ foldl1 op $ map unpackNum params

unpackNum :: Value -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch (show $ toConstr Number {}) notNum
