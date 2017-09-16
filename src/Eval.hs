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

type Primitive = (String, [Value] -> ThrowsError Value)

cons :: [Value] -> ThrowsError Value
cons [x, List xs] = return $ List $ x:xs
cons [_, notList] = notOfType "List" notList
cons args = throwError $ NumArgs 2 args

listPrimitives :: [Primitive]
listPrimitives = [("car", unaryOp unpackList id head),
                  ("cdr", unaryOp unpackList List tail),
                  ("cons", cons)]

booleanPrimitives :: [Primitive]
booleanPrimitives = [("<", binOp unpackNum Bool (<)),
                     (">", binOp unpackNum Bool (>)),
                     ("<=", binOp unpackNum Bool (<=)),
                     (">=", binOp unpackNum Bool (>=)),
                     ("&&", binOp unpackBool Bool (&&)),
                     ("||", binOp unpackBool Bool (||))]

numberPrimitives :: [Primitive]
numberPrimitives = [("+", binOp unpackNum Number (+)),
                    ("-", binOp unpackNum Number (-)),
                    ("*", binOp unpackNum Number (*)),
                    ("/", binOp unpackNum Number div),
                    ("mod", binOp unpackNum Number mod),
                    ("quotient", binOp unpackNum Number quot),
                    ("remainder", binOp unpackNum Number rem)]

typePrimitives :: [Primitive]
typePrimitives = [("symbol?", unaryOp unpackValue id $ (===) Atom {}),
                  ("list?", unaryOp unpackValue id $ (===) List {}),
                  ("number?", unaryOp unpackValue id $ (===) Number {}),
                  ("string?", unaryOp unpackValue id $ (===) String {}),
                  ("character?", unaryOp unpackValue id $ (===) Character {}),
                  ("bool?", unaryOp unpackValue id $ (===) Bool {}),
                  ("symbol->string", unaryOp unpackAtom String id)]

primitives :: [Primitive]
primitives = numberPrimitives ++
             typePrimitives ++
             listPrimitives ++
             booleanPrimitives

type Unpacker a = (Value -> ThrowsError a)
type Packer a = (a -> Value)

binOp :: Unpacker a -> Packer b -> (a -> a -> b) -> [Value] -> ThrowsError Value
binOp unpacker packer op (x1:x2:[]) = fmap packer $ liftM2 op (unpacker x1) (unpacker x2)
binOp _ _ _ list = throwError $ NumArgs 2 list

unaryOp :: Unpacker a -> Packer b -> (a -> b) -> [Value] -> ThrowsError Value
unaryOp unpacker packer op (x:[]) = fmap packer $ liftM op (unpacker x)
unaryOp _ _ _ list = throwError $ NumArgs 1 list

unpackValue :: Unpacker Value
unpackValue = return

unpackNum :: Unpacker Integer
unpackNum (Number n) = return n
unpackNum notNum = notOfType "number" notNum

unpackBool :: Unpacker Bool
unpackBool (Bool n) = return n
unpackBool notBool = notOfType "Bool" notBool

unpackAtom :: Unpacker String
unpackAtom (Atom n) = return n
unpackAtom notAtom = notOfType "Atom" notAtom

unpackList :: Unpacker [Value]
unpackList (List a) = return a
unpackList notList = notOfType "List" notList
