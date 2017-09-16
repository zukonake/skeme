module Eval (eval) where

import Control.Monad.Except
import Value
import LangError
import Primitives

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
