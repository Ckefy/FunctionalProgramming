{-# LANGUAGE InstanceSigs #-}
--Task 3.2
module Algclasses where

data NonEmpty a =
    a :| [a]
    deriving (Show)

-- | Mappend is concate of two NonEmptys

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x1 :| xs1) <> (x2 :| xs2) = x1 :| (xs1 ++ (x2 : xs2))

instance Eq a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (x1 :| xs1) == (x2 :| xs2) = ((x1 == x2) && (xs1 == xs2))

data ThisOrThat a b
   = This a
   | That b
   | Both a b
   deriving (Show)

-- | Mappend is like concating Eithers

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    Both r1 d1 <> Both r2 d2 = Both (r1 <> r2) (d1 <> d2)
    Both r1 d1 <> This r2 = Both (r1 <> r2) d1
    Both r1 d1 <> That d2 = Both r1 (d1 <> d2)
    This r1 <> Both r2 d2 = Both (r1 <> r2) d2
    This r1 <> This r2 = This (r1 <> r2)
    This r1 <> That d2 = Both r1 d2
    That d1 <> Both r2 d2 = Both r2 (d1 <> d2)
    That d1 <> This r2 = Both r2 d1
    That d1 <> That d2 = That (d1 <> d2)

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
    (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
    Both x1 y1 == Both x2 y2 = ((x1 == x2) && (y1 == y2))
    This x1 == This x2 = (x1 == x2)
    That y1 == That y2 = (y1 == y2)
    _ == _ = False

--advanced - impl Semi and Monoid
--1 Semigroup и Monoid для строк, объединяемых при помощи '.' DONE
-- ghci> Name "root" <> Name "server"
-- Name "root.server"
--2 Semigroup и Monoid для newtype Endo a = Endo { getEndo :: a -> a } DONE

--1
data Name
  = Name String
  | Empty
  deriving (Show)

-- | Mappend is concat with dot separator (except case when it`s mempty element)
instance Semigroup Name where
    (<>) :: Name -> Name -> Name
    Empty <> Name b = Name b
    Name a <> Empty = Name a
    Empty <> Empty = Empty
    Name a <> Name b = Name (a ++ ('.' : b))

instance Monoid Name where
    mempty = Empty

instance Eq (Name) where
    (==) :: Name -> Name -> Bool
    Empty == Empty = True
    Name a == Name b = a == b
    _ == _ = False

--2
-- | The monoid of endomorphisms under composition
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
    mempty = Endo id

