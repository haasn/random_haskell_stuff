{-# LANGUAGE PolyKinds, FlexibleContexts, ConstraintKinds, FlexibleInstances
  , LiberalTypeSynonyms #-}

import Prelude hiding (Monoid, (*), (+))
import qualified Prelude

import Data.Monoid (Sum(..), Product(..))

-- Just the operation
class Magma a where
  (<>) :: a -> a -> a

-- Identity: unit * x = x = x * unit
class Unital a where
  unit :: a

-- Inversion
class Quasigroup a where
  inv :: a -> a

-- Identity + inversion; inv x * x = unit = x * inv x
class (Quasigroup a, Unital a) => Loop a

-- Associativity, (x * y) * z = x * (y * z)
class Magma a => Semigroup a

-- Associativity and identity
type Monoid a = (Semigroup a, Unital a)

-- Loop with associativity
type Group a = (Semigroup a, Loop a)

-- Monoid with commutativity; x * y = y * x
class Monoid a => Commutative a

-- Group with commutativity
type Abelian a = (Group a, Commutative a)

type Additive       c x = c (Sum     x)
type Multiplicative c x = c (Product x)

-- Distribution: x * (y + z) = x * y + x * z
class (Additive Commutative a, Multiplicative Semigroup a) => Rig a
type Semiring = Rig

-- Rig with negation
type Ring a = (Rig a, Additive Abelian a)

-- Ring with commutativity and inversion
type Field a = (Ring a, Multiplicative Abelian a)

-- Friendly operators for additive and multiplicative instances

(+) :: Additive Magma a => a -> a -> a
a + b = getSum (Sum a <> Sum b)

(*) :: Multiplicative Magma a => a -> a -> a
a * b = getProduct (Product a <> Product b)

-- Instances for Double

instance Magma (Sum Double) where
  Sum a <> Sum b = Sum (a Prelude.+ b)

instance Unital (Sum Double) where
  unit = Sum 0

instance Quasigroup (Sum Double) where
  inv (Sum a) = Sum (negate a)

instance Additive Semigroup   Double
instance Additive Loop        Double
instance Additive Commutative Double

instance Magma (Product Double) where
  Product a <> Product b = Product (a Prelude.* b)

instance Unital (Product Double) where
  unit = Product 1

instance Quasigroup (Product Double) where
  inv (Product a) = Product (recip a)

instance Multiplicative Semigroup   Double
instance Multiplicative Loop        Double
instance Multiplicative Commutative Double

instance Rig Double
