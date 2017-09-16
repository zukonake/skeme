module Operator where

import Control.Monad.Except
import Value
import LangError

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

