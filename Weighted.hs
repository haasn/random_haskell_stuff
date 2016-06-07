import Data.List (permutations)
-- import System.Random

-- Pull out an element of a list and yank it to the front as well as returning

pull :: Int -> [a] -> (a, [a])
pull n xs = pull' n xs []
  where
    -- Accumulate the list in an extra buffer, similar to a zipper
    pull' _ [] _      = error "Index out of range!"
    pull' 0 (x:xs) ys = (x, x : reverse ys ++ xs)
    pull' n (x:xs) ys = pull' (n-1) xs (x:ys)

-- Isomorphism pair on indices

hither, yon :: Int -> Int
yon    = (^2)
hither = floor . (sqrt :: Double -> Double) . fromIntegral

-- Weighted random function, biased towards higher end and specialized to Ints

-- Note that for illustratory purposes I'm only using the conceptually simpler
-- but less elegant IO version of random.

{-

wrandomRIO :: (Int, Int) -> IO Int
wrandomRIO (x, y) = fmap hither (randomRIO (yon x, yon y))

pullIO :: [a] -> IO (a, [a])
pullIO xs = fmap (`pull` xs) (wrandomRIO (0, length xs - 1))

-}

-- Demo program

list = map show [1..100]

main = return ()

{-
main = go list
  where
    go xs = do
      (y, ys) <- pullIO xs
      putStrLn y
      go ys
-}
