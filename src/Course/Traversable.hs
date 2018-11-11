{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Applicative
import Course.Compose
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t =>
      Traversable t
  where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse :: Applicative f => (a -> f b) -> ExactlyOne a -> f (ExactlyOne b)
  traverse f (ExactlyOne a) = ExactlyOne <$> f a

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = Full <$> f a

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Applicative z) => (a -> z b) -> Compose f g a -> z (Compose f g b)
  traverse f (Compose t) = Compose <$> traverse (traverse f) t

-- Implement the traverse function for a Traversable instance for Compose
-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a)
          (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Product f g) where
  (<$>) :: (a -> b) -> Product f g a -> Product f g b
  f <$> Product m n = Product (f <$> m) (f <$> n)

-- Implement the (<$>) function for a Functor instance for Product
instance (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse :: (Applicative z) => (a -> z b) -> Product f g a -> z (Product f g b)
  traverse f (Product (m :: f a) (n :: g a)) = Product <$> traverse f m <*> traverse f n

-- Implement the traverse function for a Traversable instance for Product
-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a
  = InL (f a)
  | InR (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  (<$>) :: (a -> b) -> Coproduct f g a -> Coproduct f g b
  f <$> InL x = InL $ f <$> x
  f <$> InR x = InR $ f <$> x

-- Implement the (<$>) function for a Functor instance for Coproduct
instance (Traversable f, Traversable g) => Traversable (Coproduct f g)
-- Implement the traverse function for a Traversable instance for Coproduct
                                                                            where
  traverse :: (Applicative z) => (a -> z b) -> Coproduct f g a -> z (Coproduct f g b)
  traverse f (InL x) = InL <$> traverse f x
  traverse f (InR x) = InR <$> traverse f x
