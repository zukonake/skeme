{-# LANGUAGE DeriveDataTypeable #-}

module Value (Value (..), (===)) where

import Data.Data

data Value = Atom String
           | List [Value]
           | Number Integer
           | String String
           | Character Char
           | Bool Bool
           deriving (Eq, Typeable, Data)

(===) :: Value -> Value -> Value
(===) l r = Bool ((toConstr l) == (toConstr r))

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

instance Show Value where
    show (Atom val) = val ++ " ; Atom"
    show (List val) = "(" ++ unwordsList val ++ ") ; List"
    show (Number val) = show val ++ " ; Number"
    show (String val) = val ++ " ; String"
    show (Character val) = [val] ++ " ; Character"
    show (Bool val) = show val ++ " ; Bool"
