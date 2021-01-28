{-# LANGUAGE InstanceSigs #-}
module NonEmpty where

data NonEmpty a =
    a :| [a]
    deriving (Show, Eq)

--functor law:
--1 fmap id = id
--2 fmap (f . g) = fmap f . fmap g
-- | Apply func to head of NonEmpty list and 
-- concate with result of applying to tail (using Functor of list)
instance Functor NonEmpty where
   fmap f (x :| xs) = f x :| (f <$> xs)

--applicative low
--1 pure id <*> v = v
--2 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--3 pure f <*> pure x = pure (f x)
--4 u <*> pure y = pure ($ y) <*> u
-- | Reuse of Applicative instance of list - just
-- use it and concate results
instance Applicative NonEmpty where
  pure = ( :| [])
  (x1 :| xs1) <*> (x2 :| xs2) = xRes :| xsRes
    where
      (xRes : xsRes) = (x1 : xs1) <*> (x2 : xs2)

-- | Same implementation of foldr as in list
instance Foldable NonEmpty where
  foldr f start (x :| xs) = f x (foldr f start xs)

--make <$> to get function x :|
--so we have f (a -> b) and f(a), apply with <*>
instance Traversable NonEmpty where
  traverse :: Applicative f1 => (a1 -> f1 b1) -> NonEmpty a1 -> f1(NonEmpty b1)
  traverse f (x :| xs) = (:|) <$> f x <*> other
    where
      other = traverse f xs

instance Monad NonEmpty where
  (x :| xs) >>= f = xRes :| xsRes
     where
       --make first elem
       (xRes :| xsEmpty) = f x
       --make list out of NonEmpty
       makeList (temp :| temps) = temp : temps
       --takes xsEmpty and add to it list (which we get from binding tail (xs))
       xsRes = xsEmpty ++ (xs >>= (makeList . f))


