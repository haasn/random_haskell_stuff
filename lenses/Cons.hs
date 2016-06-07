{-# LANGUAGE Rank2Types #-}

import Control.Lens

(.:) :: Simple Lens a c -> Simple Lens a [c] -> Simple Lens a [c]
x .: xs = lens get set
  where
    get a        = a^.x : a^.xs
    set _ []     = undefined
    set a (c:cs) = x .~ c $ xs .~ cs $ a
