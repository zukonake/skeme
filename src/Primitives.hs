module Primitives where

import Control.Monad.Except
import Value
import LangError
import Operator

type Primitive = [Value] -> ThrowsError Value

cons :: Primitive
cons [x, List xs] = return $ List $ x:xs
cons [_, notList] = notOfType "List" notList
cons args = throwError $ NumArgs 2 args

listPrimitives :: [(String, Primitive)]
listPrimitives = [("car", unaryOp unpackList id head),
                  ("cdr", unaryOp unpackList List tail),
                  ("cons", cons)]

booleanPrimitives :: [(String, Primitive)]
booleanPrimitives = [("<", binOp unpackNum Bool (<)),
                     (">", binOp unpackNum Bool (>)),
                     ("<=", binOp unpackNum Bool (<=)),
                     (">=", binOp unpackNum Bool (>=)),
                     ("&&", binOp unpackBool Bool (&&)),
                     ("||", binOp unpackBool Bool (||))]

numberPrimitives :: [(String, Primitive)]
numberPrimitives = [("+", binOp unpackNum Number (+)),
                    ("-", binOp unpackNum Number (-)),
                    ("*", binOp unpackNum Number (*)),
                    ("/", binOp unpackNum Number div),
                    ("mod", binOp unpackNum Number mod),
                    ("quotient", binOp unpackNum Number quot),
                    ("remainder", binOp unpackNum Number rem)]

typePrimitives :: [(String, Primitive)]
typePrimitives = [("symbol?", unaryOp unpackValue id $ (===) Atom {}),
                  ("list?", unaryOp unpackValue id $ (===) List {}),
                  ("number?", unaryOp unpackValue id $ (===) Number {}),
                  ("string?", unaryOp unpackValue id $ (===) String {}),
                  ("character?", unaryOp unpackValue id $ (===) Character {}),
                  ("bool?", unaryOp unpackValue id $ (===) Bool {}),
                  ("symbol->string", unaryOp unpackAtom String id)]

lispIf :: Primitive
lispIf [Bool True, true, false] = return $ true
lispIf [Bool False, true, false] = return $ false
lispIf (nonBool:_) = notOfType "Bool" nonBool
lispIf args = throwError $ NumArgs 3 args

conditionalPrimitives :: [(String, Primitive)]
conditionalPrimitives = [("if", lispIf)]

primitives :: [(String, Primitive)]
primitives = numberPrimitives ++
             typePrimitives ++
             listPrimitives ++
             booleanPrimitives ++
             conditionalPrimitives
