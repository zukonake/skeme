module LangError where  
import Control.Monad.Error
import Value
import Text.Parsec.Error

data LangError = NumArgs Integer [Value]
            | TypeMismatch String Value
            | Parser ParseError
            | BadSpecialForm String Value
            | NotFunction String String
            | UnboundVar String String
            | Default String

instance Show LangError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected 
                                           ++ " args"
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                           ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr 
    
instance Error LangError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LangError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
