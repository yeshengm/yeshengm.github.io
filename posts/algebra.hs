{-# LANGUAGE NoImplicitPrelude #-}
module Algebra where

import           Data.List (foldr)
import           Prelude   (undefined)

-- semigroup: a binary associative operation.
--  associativity: (x <> y) <> z = x <> (y <> z)
class Semigroup a where
  (<>) :: a -> a -> a

-- monoid: a binary associative operation with identity.
--  identity: x <> mempty = x ; mempty <> x = x
--  associativity: (x <> y) <> z = x <> (y <> z)
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = Data.List.foldr mappend mempty

-- functor: function application over structure.
--  identity: fmap id = id
--  composition: fmap f . fmap g = fmap (f . g)
class Functor f where
  fmap :: (a -> b) -> f a -> f b -- lift

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- applicative: a monoidal functor.
--  identity: pure id <*> v = v
--  composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--  homomorphism: pure f <*> pure x = pure (f x)
--  interchange: u <*> pure y = pure ($ y) <*> u
class Functor f => Applicative f where
  pure :: a -> f a
  (*>) :: f a -> f b -> f b
  (<*>) :: f (a -> b) -> f a -> f b

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = undefined
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = undefined
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = undefined

-- monad:
--  identity: m >>= return = m ; return x >>= f = f x
--  associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
  (>>) :: m a -> m b -> m b
  (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

join :: Monad m => m (m a) -> m a
join = undefined
