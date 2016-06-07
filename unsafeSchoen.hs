{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module UnsafeSchoen where

import Control.Arrow
import Control.Category (Category)
import Unsafe.Coerce

newtype S c a b = S { s :: c a b }
  deriving (Category, Arrow)

-- This is the old instance GNT would have written, which is illegal now
instance ArrowApply c => ArrowApply (S c) where
  app = (unsafeCoerce :: c (c a b, a) b -> S c (S c a b, a) b) app

data Bad a b = Bad { bad :: IfS a b }

type family IfS a b where
  IfS (S Bad x y, a) b = a -> b
  IfS (Bad x y  , a) b = a -> a

instance Category Bad
instance Arrow Bad
instance ArrowApply Bad where
  app = Bad id

oops :: a -> b
oops = bad (s app)
