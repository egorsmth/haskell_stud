data List a = Cons a (List a) | HappyEnd

instance (Show a) => Show (List a) where
  show HappyEnd = "()"
  show (Cons a b) = "(" ++ show a ++ " " ++ show b ++ ")"

append :: List a -> List a -> List a
append (Cons a HappyEnd) b = Cons a b
append HappyEnd b = b
append (Cons a as) b = Cons a (append as b)

instance Functor List where
  --fmap :: (a -> b) -> f a -> f b
  fmap _ HappyEnd = HappyEnd
  fmap f (Cons x ys) = Cons (f x) (fmap f ys)
  --(<$) :: a -> f b -> f a
  (<$) _ HappyEnd = HappyEnd
  (<$) a (Cons x xs) = Cons a (a <$ xs)


instance Applicative List where
  -- pure :: a -> f a
  pure a = Cons a HappyEnd
  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) _ HappyEnd = HappyEnd
  (<*>) HappyEnd _ = HappyEnd
  (<*>) (Cons f fs) (Cons x xs) =  append (fmap f (Cons x xs)) (fs <*> (Cons x xs))




main = do
   print $ (+) <$> (Cons 2 (Cons 4 HappyEnd)) <*> (Cons 4 (Cons 7 HappyEnd))
   print $ (Cons id (Cons id HappyEnd)) <*> (Cons 4 (Cons 7 HappyEnd))
  --Identity pure id <*> v = v
   print $ show (pure id <*> a) ++ " should be " ++ show a
  --Composition pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   print $ show (pure (.) <*> u <*> v <*> a) ++ " should be " ++ show (u <*> (v <*> a))
  --Homomorphism pure f <*> pure x = pure (f x)
   --print $ show (pure f <*> pure x) ++ " should be " ++ show (pure (f x))
   print $ pure (f x)
  --Interchange u <*> pure y = pure ($ y) <*> u
   print $ show (u <*> pure y) ++ " should be " ++ show (pure ($ y) <*> u)
 where
   a = Cons 2 (Cons 4 HappyEnd)
   u = fmap (+) (Cons 7 (Cons 11 HappyEnd))
   v = fmap (+) (Cons 7 (Cons 11 HappyEnd))
   f = (+ 1)
   y = 9
   x = 66