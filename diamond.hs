import Data.List (inits, tails)

mirror :: [a] -> [a]
mirror xs = xs ++ tail (reverse xs)

slope :: Int -> [String]
slope n = zipWith (++) (tails $ replicate n ' ') (inits $ replicate n '*')

diamond :: Int -> String
diamond = unlines . mirror . map mirror . tail . slope

main = putStr $ diamond 9
