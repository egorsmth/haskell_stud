module QuadPolyTypes (SqEqAnswer(..)) where

data SqEqAnswer = TwoRoots Double Double | OneRoot Double | NoRoots | InfRoots
  deriving (Eq)

instance Show SqEqAnswer where
  show (TwoRoots a b) = "Ответ: x1=" ++ show a ++ ", x2=" ++ show b
  show (OneRoot a) = "Ответ: x=" ++ show a
  show (NoRoots) = "Нет корней"
  show (InfRoots) = "Бесконечное количество корней"