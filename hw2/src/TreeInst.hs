module TreeInst where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

--functor law:
--1 fmap id = id
--2 fmap (f . g) = fmap f . fmap g
-- | Apply function to inner value of Tree
instance Functor Tree where
   fmap f (Leaf a) = Leaf $ f a
   fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

--applicative low
--1 pure id <*> v = v
--2 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--3 pure f <*> pure x = pure (f x)
--4 u <*> pure y = pure ($ y) <*> u
-- | Apply function in Tree to inner value of Tree
instance Applicative Tree where
   Leaf f <*> Leaf a = Leaf (f a)
   Branch f1 f2 <*> Branch a b = Branch (f1 <*> a) (f2 <*> b)
   Branch f1 f2 <*> aOut@(Leaf a) = Branch (f1 <*> aOut) (f2 <*> aOut)
   fOut@(Leaf f) <*> Branch a b = Branch (fOut <*> a) (fOut <*> b)
   pure = Leaf

-- | Foldr implementation of Tree - begin with right child recursively fold
instance Foldable Tree where
   foldr f start (Leaf a) = f a start
   foldr f start (Branch a b) = foldr f (foldr f start b) a

-- | Traversable insstance for Tree
instance Traversable Tree where
   traverse f (Leaf a) = Leaf <$> f a
   traverse f (Branch a b) = Branch <$> traverse f a <*> traverse f b






