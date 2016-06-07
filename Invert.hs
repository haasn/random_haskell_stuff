import Data.Tuple (swap)

invert :: ((Int,  Int) -> Int) -> Int -> [(Int,Int)]
invert f z = go (first 0)

  where
    first v
      | f (0,v) < z = first (v+1)
      | otherwise   = (0,v)

    go p@(u,v)
      -- Terminating conditions
      | v < 0 = [] -- Escapes Nat
      | u > z = [] -- No more solution possible

      -- Stepping
      | otherwise = case compare (f p) z of
        GT ->     go (u  ,v-1) -- Too big, step down
        EQ -> p : go (u+1,v-1) -- Just right, go diagonally
        LT ->     go (u+1,v+1) -- Too small, try next row




-- Testing

f0 (x,y) = 2^y*(2*x + 1) - 1
f1 (x,y) = x*2^x + y*2^y + 2*x + y
f2 (x,y) = 3*x + 27*y + y^2
f3 (x,y) = x^2 + y^2 + x + y
f4 (x,y) = x + 2^y + y - 1

-- Flipped functions

f0' = f0 . swap
f1' = f1 . swap
f2' = f2 . swap
f3' = f3 . swap
f4' = f4 . swap
