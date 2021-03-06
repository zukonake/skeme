module Parser (readExpr) where

import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import Value
import LangError
import Control.Monad.Except

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError Value
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

parseAtom :: Parser Value
parseAtom = do
    quote <- optionMaybe $ char '\''
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> if quote == Nothing
                 then Atom atom
                 else List $ [Atom "quote", Atom atom]

readBin :: Integral a => String -> a
readBin [] = 0
readBin val = if (last val) == '1'
    then (((readBin . init) val) * 2) + 1
    else (((readBin . init) val) * 2)

parseRadixLiteral :: Parser Value
parseRadixLiteral = do
    char '#'
    prefix <- oneOf "bodx"
    rest <- parseLiteral prefix
    return $ Number (rest)
    where parseLiteral :: Char -> Parser Integer
          parseLiteral prefix = case prefix of
              'b' -> many1 (oneOf "01") >>= (return . readBin)
              'o' -> many1 octDigit >>= (return . fst . head . readOct)
              'd' -> many1 digit >>= (return . read)
              'x' -> many1 hexDigit >>= (return . fst . head . readHex)
              _   -> fail "Invalid radix prefix"

parseInteger :: Parser Value
parseInteger = parseRadixLiteral <|>
    (do
    value <- many1 digit
    return $ (Number . read) value)

parseCharacter :: Parser Value
parseCharacter = do
    char '#'
    char '\\'
    val <- (string "space") <|> (string "newline") <|> (many1 letter)
    case val of
        "space"   -> (return . Character) ' '
        "newline" -> (return . Character) '\n'
        _         -> (return . Character) (head val)

parseList :: Parser Value
parseList = do
    quote <- optionMaybe $ char '\''
    char '('
    val <- sepBy parseExpr spaces
    char ')'
    if quote == Nothing
        then return $ List val
        else return $ List $ [Atom "quote", List val]

parseExpr :: Parser Value
parseExpr = try parseCharacter
    <|> try parseString
    <|> try parseInteger
    <|> try parseAtom
    <|> try parseList

parseString :: Parser Value
parseString = do
    char '"'
    val <- many $ (char '\\' >> oneOf "\"nrt\\") <|> noneOf "\""
    char '"'
    return $ String val
