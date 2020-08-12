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

showVal :: LispVal -> String
showVal (String c) = "\"" ++ c ++ "\""
showVal (Atom c) = c
showVal (Number c) = show c
showVal (Bool False) = "#f"
showVal (Bool True) = "#t"
showVal (Float a) = show a
showVal (Character a) = "#\\" ++ a
showVal (List x) = "(" ++ (unwords $ map showVal x) ++ ")"
showVal (DottedList x y) = "(" ++ (unwords $ map showVal x) ++ " . " ++ showVal y ++ ")"

instance Show LispVal where show = showVal



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


eval :: LispVal -> LispVal
eval val@(List [Atom "quote", a]) = a
eval (List (Atom func : args)) = apply func $ map eval args
eval a = a

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", binop isNumber)]

numericBinop op params = Number $ foldl1 op $ map (\(Number x) -> x) params
binop op params = Bool $ foldl1 (&&) $ map op params
isNumber (Number _) = True
isNumber _ = False


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right a -> a
    

main :: IO ()
main = do
    (a:_) <- getArgs
    putStrLn (show $ eval $ readExpr a)