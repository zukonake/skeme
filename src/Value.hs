{-# LANGUAGE DeriveDataTypeable #-}
module Value ( Value (..), (===)) where
import Data.Data
import Data.Typeable

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
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
    show (DottedList list val) = "(" ++ unwordsList list ++ " . " ++ show val ++ ") ; DottedList"
    show (Number val) = show val ++ " ; Number"
    show (String val) = val ++ " ; String"
    show (Character val) = [val] ++ " ; Character"
    show (Bool val) = show val ++ " ; Bool"
