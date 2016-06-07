import           Control.Comonad
import           Data.List.Zipper

instance Extend Zipper where
  duplicate = duplicatez
  extend    = extendz

instance Comonad Zipper where
  extract   = cursor
