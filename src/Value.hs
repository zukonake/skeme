module Value where

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Character Char
           | Bool Bool

instance Show Value where
    show (Atom val) = val
    show (List (x:xs)) = show x ++ ", " ++ show xs
    show (DottedList (x:xs) val) = "TODO"
    show (Number val) = show val
    show (String val) = val
    show (Character val) = [val]
    show (Bool val) = show val
