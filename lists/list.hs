data List a = Cons (List a) (List a) | Val a | HappyEnd 

showTail HappyEnd = ""
showTail (Cons a b) = " " ++ show a ++ showTail b
showTail (Val a) = " . " ++ show a

instance (Show a) => Show (List a) where
  show HappyEnd = ""
  show (Val a) = show a
  show (Cons a b) = "(" ++ show a ++ showTail b ++ ")"

append :: List a -> List a -> List a
append (Cons a HappyEnd) b = Cons a b
append HappyEnd b = b
append (Cons a as) b = Cons a (append as b)

instance Functor List where
  --fmap :: (a -> b) -> f a -> f b
  fmap _ HappyEnd = HappyEnd
  fmap f (Val a) = Val (f a)
  fmap f (Cons x y) = Cons (fmap f x)  (fmap f y)
  --(<$) :: a -> f b -> f a
  (<$) _ HappyEnd = HappyEnd
  (<$) a (Val x) = Val a
  (<$) a (Cons x y) = Cons (a <$ x) (a <$ y) 


instance Applicative List where
  pure a = Val a
  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) _ HappyEnd = HappyEnd
  (<*>) HappyEnd _ = HappyEnd
  (<*>) (Val f) (Val a) = Val (f a)
  (<*>) (Cons fx fy) (Val a) = Cons (fx <*> (Val a)) (fy <*> (Val a))
  (<*>) (Val f) (Cons ax ay) = Cons ((Val f) <*> ax) ((Val f) <*> ay)
  (<*>) (Cons fx fy) (Cons ax ay) = append (fx <*> (Cons ax ay)) (fy <*> (Cons ax ay))

main = do
    print $ (+) <$> [2,4,5] <*> [4,7]
    print $ ((+) <$> (Cons (Val 2) (Cons (Val 4) HappyEnd))) <*> (Cons (Val 4) (Cons (Val 7) HappyEnd))
    print $ (Cons (Val 2) (Cons (Val 4) (Val 5)))
    print $ ((+) <$> (Cons (Val 2) (Cons (Val 4) HappyEnd))) <*> (Cons (Val 2) (Cons (Cons (Val 4) (Cons (Val 99) HappyEnd)) (Val 5)))
