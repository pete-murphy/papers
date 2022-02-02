{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (Num (..))
import qualified Prelude

-- A particular novelty of this paper is the application of ideas from the theory
-- of relational databases to the design of type systems.

-- In most implementations, the presence of a predicate in a function’s type
-- indicates that an implicit parameter will be added to pass some appropriate
-- evidence for that predicate at run-time.

-- For example, what might a predicate of the form _R a b_ mean, where two
-- parameters a and b have been provided? The obvious answer is to interpret R
-- as a two-place relation between types, and to read _R a b_ as the assertion
-- that a and b are related by R. This is a natural generalization of the one
-- parameter case because sets are just one-place relations

-- subtyping relation
class Coerce a b where
  coerce :: a -> b

-- If we have
-- instance Eq Bool
-- and
-- instance (Eq a, Eq b) => Eq (a, b)
-- then we also get all these
-- instance Eq (Bool, Bool)
-- instance Eq ((Bool, Bool), Bool)
-- instance Eq (((Bool, Bool), Bool), Bool)
-- instance Eq (Bool, (((Bool, Bool), Bool), Bool))

class Collects e ce where
  empty :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool

f x y coll = insert x (insert y coll)

-- This compiles but shouldn't!
g coll = f True 'a' coll

instance Eq e => Collects e [e] where
  empty = []
  insert = (:)
  member = elem

x :: [Int]
x = empty @Int

----

-- Possible solution
-- A "constructor class"
class Collects' e c where
  empty' :: c e
  insert' :: e -> c e -> c e
  member' :: e -> c e -> Bool

-- This is ugly, compared to
-- instance Eq e ⇒ Collects e (e → Bool ) where
newtype CharFun e = MkCharFun (e -> Bool)

instance Eq e => Collects' e CharFun where
  empty' = MkCharFun (const False)
  insert' e (MkCharFun f) = MkCharFun (\e' -> e == e' || f e')
  member' e (MkCharFun f) = f e

-- Also doesn't work for some types like BitSet where
-- element type is fixed

-- With Functional Dependencies

class Collects'' e c | c -> e where
  empty'' :: c e
  insert'' :: e -> c e -> c e
  member'' :: e -> c e -> Bool

class D a b | a -> b

-- A more flexible approach than Num type class
class Add a b c | a b -> c where
  (+) :: a -> b -> c

class Mul a b c | a b -> c where
  (*) :: a -> b -> c

instance Mul Int Int Int where
  (*) = (Prelude.*)

instance Mul Int Float Float where
  n * f = (Prelude.*) (fromIntegral n) f

instance Mul Float Int Float where
  f * n = (Prelude.*) f (fromIntegral n)

instance Mul Float Float Float where
  (*) = (Prelude.*)
