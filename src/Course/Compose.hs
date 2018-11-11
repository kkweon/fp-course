{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.
newtype Compose f g a =
  Compose (f (g a)) deriving (Eq, Show)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  h <$> Compose x = Compose ((h <$>) <$> x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose (f :: f (g (a -> b))) <*> Compose (x :: f (g a)) =
    Compose $ lift2 (<*>) f x

-- Implement the pure function for an Applicative instance for Compose
-- Implement the (<*>) function for an Applicative instance for Compose
instance (Monad f, Monad g) => Monad (Compose f g)
-- Implement the (=<<) function for a Monad instance for Compose
                                                                 where
  (=<<) = error "todo: Course.Compose (<<=)#instance (Compose f g)"
