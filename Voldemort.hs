import Data.Bits (shiftR)

type InputRange = [] -- An odd name for lists

-- We'll go with an algorithm like the one in the article

rng :: Int -> InputRange Int
rng seed = next : rng (seed * 1103515245 + 12345)
  where next = (seed `div` 0x10000) * seed `shiftR` 16

-- Print out 10 such numbers

main = mapM_ print . take 10 $ rng 5
