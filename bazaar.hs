import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Bazaar
import Data.Semigroup

-- Const orphan instances
instance Semigroup a => Semigroup (Const a b) where
  Const a <> Const b = Const (a <> b)

instance Monoid a => Monoid (Const a b) where
  mempty = Const mempty
  mappend (Const a) (Const b) = Const (mappend a b)

-- Safe Bazaar instances
instance Semigroup t => Semigroup (Bazaar p a b t) where
  Bazaar a <> Bazaar b = Bazaar $ \f -> liftA2 (<>) (a f) (b f)

instance Monoid t => Monoid (Bazaar p a b t) where
  mempty = Bazaar $ \_ -> pure mempty
  mappend (Bazaar a) (Bazaar b) = Bazaar $ \f -> liftA2 mappend (a f) (b f)

-- Unsafe BazaarT instances, rely on Contravariant
instance Contravariant g => Semigroup (BazaarT p g a b t) where
  BazaarT a <> BazaarT b = BazaarT $ \f -> a f <* b f

instance Contravariant g => Monoid (BazaarT p g a b t) where
  mempty = BazaarT $ \_ -> pure (error "mempty: BazaarT")
  mappend = (<>)
