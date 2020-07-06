lineSolver :: Float -> Float -> Float
lineSolver b c 
    | b == 0 = c
    | otherwise = -c / b

quadPolySolver :: Float -> Float -> Float -> [Float]
quadPolySolver a b c
    | a == 0 = [lineSolver b c]
    | otherwise = inner a b c
    where 
        d = b*b - 4 * a * c
        inner a b c
            | d < 0     = []
            | d == 0    = [(-b + sqrt d) / (2 * a)]
            | otherwise = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
            
        
fib :: Int -> Int
fib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

fibTailed :: Int -> Int
fibTailed n = fibInner 0 1 n
    where 
        fibInner prev cur n 
            | n == 0 = cur 
            | otherwise = fibInner cur (prev+cur) (n-1)

main = print $ quadPolySolver 1 (-3) (-4)