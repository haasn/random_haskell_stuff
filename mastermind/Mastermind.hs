import Control.Applicative
import Control.Comonad

data Color = Red | Green | Blue | Yellow | White | Black deriving (Eq, Show)

answer :: [Color]
answer = [Blue, Yellow, White, Black]

points :: [Color] -> Int
points = (+) <$> tens <*> ones
  where tens = (*10) . length . filter id . zipWith (==) answer
