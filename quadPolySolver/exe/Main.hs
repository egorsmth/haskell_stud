import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Options.Applicative
import Control.Applicative ((<**>))

import QuadPoly (quadPolySolver)

isExitCommand c = any (==c) ["exit", "quit", "q"]

mainLoop = do
  putStrLn "This is quadratic polynomial solver"
  putStrLn "Enter the arguments a b c"
  line <- getLine
  if (length $ words line) == 1 && isExitCommand line
  then return ()
  else if (length $ words line) /= 3
  then do
      putStrLn "Not enough arguments"
      mainLoop
  else do
      let [a,b,c] = map read $ words line
      putStrLn $ show (quadPolySolver a b c)
      mainLoop

newTest = do
    putStrLn $ show (quadPolySolver 1 2 1)
    putStrLn $ show (quadPolySolver 1 5 1)
    putStrLn $ show (quadPolySolver 0 0 1)
    putStrLn $ show (quadPolySolver 0 0 0)

data Options = Options {a, b, c :: Double}

parser :: Parser Options
parser = Options
  <$> option auto
    ( long "arg a"
    <> short 'a'
    <> help "fiirst arg a"
    )
  <*> option auto
    ( long "arg b"
    <> short 'b'
    <> help "second arg b"
    )
  <*> option auto
    ( long "arg c"
    <> short 'c'
    <> help "third arg c")

parserInfo = 
  info
    (parser <**> helper)
    (   fullDesc
    <>  progDesc "Square root equation solver"
    <>  header "Hi!")

main = do
  Options{a = a, b = b, c = c} <- execParser parserInfo
  putStrLn $ show (quadPolySolver a b c)


