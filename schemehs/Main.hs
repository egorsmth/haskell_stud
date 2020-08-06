module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character String
             | Float Float

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseCharacter :: Parser LispVal
parseCharacter = do
    char '#'
    char '\\'
    x <- many (letter <|> digit <|> symbol)
    return $ Character x

parseNumber :: Parser LispVal
parseNumber = do 
    a <- many1 digit
    return $ (Number . read) a

parseFloat :: Parser LispVal
parseFloat = do 
    a <- many1 digit
    char '.' 
    b <- many1 digit
    let atom = a++"."++b
    return $ Float  (read atom :: Float)

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
         <|> parseAtom
         <|> parseString
         <|> try parseFloat
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]




readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (Atom a) -> "Found atom: " ++ a
    Right (String a) -> "Found string: " ++ a
    Right (Number a) -> "Found number: " ++ show a
    Right (Float a) -> "Found float: " ++ show a
    Right (Character a) -> "Found Character: " ++ a

main :: IO ()
main = do
    (a:_) <- getArgs
    putStrLn (readExpr a)