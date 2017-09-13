module Eval (eval) where

import Value
import LangError
import Control.Monad.Except

eval :: Value -> ThrowsError Value

-- things that eval to themselves: all atoms and quoted things
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
-- unquoted lists are function applications
--  and eval to the result of the application
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [Value] -> ThrowsError Value
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args) $ lookup func primitives

car :: Value -> ThrowsError Value
car (List (x:_)) = return x
car v = notOfType "List" v

cdr :: Value -> ThrowsError Value
cdr (List (_:xs)) = return $ List xs
cdr v = notOfType "List" v

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
symbolToString (Atom str) = return $ String str
symbolToString notSym = throwError $ TypeMismatch "symbol" notSym


notOfType :: String -> Value -> ThrowsError Value
notOfType typeName val = throwError $ TypeMismatch typeName val

monadicEquals :: Value -> Value -> ThrowsError Value
monadicEquals val1 val2 = return $ (===) val1 val2

typePrimitives :: [Primitive]
typePrimitives = [("symbol?", unaryOp $ monadicEquals $ Atom {}),
                  ("list?", unaryOp $ monadicEquals $ List {}),
                  ("number?", unaryOp $ monadicEquals $ Number {}),
                  ("string?", unaryOp $ monadicEquals $ String {}),
                  ("character?", unaryOp $ monadicEquals $ Character {}),
                  ("bool?", unaryOp $ monadicEquals $ Bool {}),
                  ("symbol->string", unaryOp symbolToString)]

primitives :: [Primitive]
primitives = numberPrimitives ++
             typePrimitives ++
             listPrimitives

unaryOp :: (Value -> ThrowsError Value) -> [Value] -> ThrowsError Value
unaryOp op [x] = op x
unaryOp _ list = throwError $ NumArgs 1 list


numericBinOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
numericBinOp op params = fmap Number $ foldl1 (liftM2 op) $ map unpackNum params

unpackNum :: Value -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
