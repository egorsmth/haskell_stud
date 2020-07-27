import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import QuadPoly (quadPolySolver)
import QuadPolyTypes (SqEqAnswer(..))

main = defaultMain $ testGroup "Square equations" [
  testCase "x^2 + 2x + 1 = 0 -> -1" (quadPolySolver 1 2 1 @?= OneRoot (-1)),
  testCase "x^2 + 5x + 1 = 0 -> -0.2087121, -4.791288" (quadPolySolver 1 5 1 @?= TwoRoots (-0.2087121) (-4.791288)),
  testCase "2x + 1 = 0 -> -0.5" (quadPolySolver 0 2 1 @?= OneRoot (-0.5)),
  testCase "1 = 0 -> No roots" (quadPolySolver 0 0 1 @?= NoRoots),
  testCase "0 = 0 -> Inf" (quadPolySolver 0 0 0 @?= InfRoots)
  ]