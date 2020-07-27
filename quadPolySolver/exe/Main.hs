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

main = mainLoop

