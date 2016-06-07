{-# LANGUAGE Rank2Types #-}

import Prelude hiding (any)

import Control.Monad.Identity
import Control.Applicative

import Data.List ((\\), transpose)
import Data.List.Split (splitEvery)

-- Type for the two-dimensional field

type Field a = [[a]]

row :: Int -> Field a -> [a]
row n field = field !! n

col :: Int -> Field a -> [a]
col n field = transpose field !! n

pos :: (Int, Int) -> Field a -> a
pos (x, y) field = (field !! y) !! x

box :: (Int, Int) -> Field a -> Field a
box (x, y) = map (get x) . get y
  where get p = row (p `div` 3) . splitEvery 3

mapWithPos :: ((Int, Int) -> a -> b) -> Field a -> Field b
mapWithPos f = zipWith (\y -> zipWith (\x -> f (x,y)) [0..]) [0..]


-- Type for known or partially known fields

data Sudoku
  = K { unK :: Int   } -- ^ Known
  | U { unU :: [Int] } -- ^ Unknown
  deriving (Show, Eq, Read)

-- Pretty printer
pretty :: Field Sudoku -> IO ()
pretty = mapM_ (\x -> do mapM_ printRow x; putStrLn "") . splitEvery 3
  where
    printRow :: [Sudoku] -> IO ()
    printRow row = do
      mapM_ (\x -> do mapM_ printSudoku x; putStr " ") $ splitEvery 3 row
      putStrLn ""

    printSudoku :: Sudoku -> IO ()
    printSudoku (K x) = putStr (show x)
    printSudoku (U _) = putStr "_"

any :: Sudoku
any = U [1..9]

known :: [Sudoku] -> [Int]
known = map unK . filter isK
  where isK (K _) = True
        isK (U _) = False


-- Solving algorithms

-- Limit all unknowns to restrictions imposed by knowns per row/col/box
limit :: Field Sudoku -> Field Sudoku
limit f = restrict . flip mapWithPos f $ \(x,y) s -> case s of
  K x  -> K x
  U xs -> U $ ((xs \\ known (row y f))
                   \\ known (col x f))
                   \\ known (join $ box (x,y) f)

-- Restrict fields that have been limited to just one possibility
restrict :: Field Sudoku -> Field Sudoku
restrict = mapWithPos $ \_ x -> case x of
  K  x  -> K x
  U [x] -> K x
  U  xs -> U xs

-- A solve step, currently only the limit algorithm is applied
step :: Field Sudoku -> Field Sudoku
step = limit

-- Try to solve as far as possible by repeatedly applying ‘step’
trySolve :: Field Sudoku -> Field Sudoku
trySolve f
  | f == f'   = f
  | otherwise = trySolve f'
  where f' = step f

-- Example field

example :: Field Sudoku
example =
  [ [K 5, K 3, any,  any, K 7, any,  any, any, any]
  , [K 6, any, any,  K 1, K 9, K 5,  any, any, any]
  , [any, K 9, K 8,  any, any, any,  any, K 6, any]

  , [K 8, any, any,  any, K 6, any,  any, any, K 3]
  , [K 4, any, any,  K 8, any, K 3,  any, any, K 1]
  , [K 7, any, any,  any, K 2, any,  any, any, K 6]

  , [any, K 6, any,  any, any, any,  K 2, K 8, any]
  , [any, any, any,  K 4, K 1, K 9,  any, any, K 5]
  , [any, any, any,  any, K 8, any,  any, K 7, K 9]
  ]

example2 :: Field Sudoku
example2 =
  [ [K 8, any, any,  any, any, any,  any, any, any]
  , [any, any, K 3,  K 6, any, any,  any, any, any]
  , [any, K 7, any,  any, K 9, any,  K 2, any, any]

  , [any, K 5, any,  any, any, K 7,  any, any, any]
  , [any, any, any,  any, K 4, K 5,  K 7, any, any]
  , [any, any, any,  K 1, any, any,  any, K 3, any]

  , [any, any, K 1,  any, any, any,  any, K 6, K 8]
  , [any, any, K 8,  K 5, any, any,  any, K 1, any]
  , [any, K 9, any,  any, any, any,  K 4, any, any]
  ]
