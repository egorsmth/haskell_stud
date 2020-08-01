module QuadPoly (quadPolySolver) where

import QuadPolyTypes (SqEqAnswer(..))

lineSolver :: Double -> Double -> SqEqAnswer
lineSolver b c 
    | b == 0 && c == 0 = InfRoots
    | b == 0 = NoRoots
    | otherwise = OneRoot (-c / b)

quadPolySolver :: Double -> Double -> Double -> SqEqAnswer
quadPolySolver a b c
    | a == 0 = lineSolver b c
    | otherwise = inner a b c
  where 
    d = b*b - 4 * a * c
    inner a b c
        | d < 0     = NoRoots
        | d == 0    = OneRoot ((-b + sqrt d) / (2 * a))
        | otherwise = TwoRoots ((-b + sqrt d) / (2 * a)) ((-b - sqrt d) / (2 * a))