module Value ( Value (..)) where

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Character Char
           | Bool Bool

showListContents :: Show a => [a] -> String
showListContents val = tail (foldr (++) [] (map ((++) " " . show) val))

instance Show Value where
    show (Atom val) = val
    show (List val) = "(" ++ showListContents val ++ ")"
    show (DottedList list val) = "(" ++ showListContents list ++ " . " ++ show val ++ ")"
    show (Number val) = show val
    show (String val) = val
    show (Character val) = [val]
    show (Bool val) = show val
