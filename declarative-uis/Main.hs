{-# LANGUAGE FunctionalDependencies, RankNTypes #-}

import Data.Functor.Identity
import Control.Comonad

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> c) -> f a -> g b -> c

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

-- newtype Id a = Id a
-- 
-- instance Pairing Identity Id where
--   pair f (Identity a) (Id b) = f a b
-- 
-- declarative-uis/Main.hs:8:10-34: error:
--     Functional dependencies conflict between instance declarations:
--       instance Pairing Identity Identity
--         -- Defined at declarative-uis/Main.hs:8:10
--       instance Pairing Identity Id
--         -- Defined at declarative-uis/Main.hs:13:10
--   |
-- 8 | instance Pairing Identity Identity where
--   |          ^^^^^^^^^^^^^^^^^^^^^^^^^
--
-- *************************************************
--
-- In PureScript the error looks like
--   Overlapping type class instances found for
-- 
--     Notes.Pairing Identity
--                   Id
-- 
--   The following instances were found:
-- 
--     instance in module Notes with type Pairing Identity Identity (line 10, column 1 - line 11, column 43)
--     instance in module Notes with type Pairing Identity Id (line 15, column 1 - line 16, column 37)

instance Pairing ((->) a) ((,) a) where
  pair f g (a, b) = f (g a) b

newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }

-- instance Comonad w => Monad (Co w)

main :: IO ()
main = pure ()
