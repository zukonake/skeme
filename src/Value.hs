module Value ( Value (..)) where

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Character Char
           | Bool Bool

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

instance Show Value where
    show (Atom val) = val
    show (List val) = "(" ++ unwordsList val ++ ")"
    show (DottedList list val) = "(" ++ unwordsList list ++ " . " ++ show val ++ ")"
    show (Number val) = show val
    show (String val) = val
    show (Character val) = [val]
    show (Bool val) = show val
