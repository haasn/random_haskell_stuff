import Control.Applicative
import Control.Lens
import Control.Monad.Loops
import System.Random

data RPS = Rock | Paper | Scissors
  deriving (Show, Read, Eq, Enum, Bounded)

beats :: RPS -> RPS -> Bool
Rock     `beats` Scissors = True
Scissors `beats` Paper    = True
Paper    `beats` Rock     = True
_        `beats` _        = False

instance Random RPS where
  randomR (l,h) = over _1 toEnum . randomR (fromEnum l, fromEnum h)
  random = randomR (minBound, maxBound)

type Score = (Int, Int)

game :: IO Score
game = iterateUntilM ((>=2) . uncurry max) ?? (0,0) $ \(h,c) -> do
  w <- liftA2 beats (read <$> getLine) randomIO
  return $ if w then (h+1,c) else (h,c+1)

main = do
  (h,c) <- game
  putStrLn $ "Human: " ++ show h
  putStrLn $ "Computer: " ++ show c
  let winner | h > c = "Human" | otherwise = "Computer"
  putStrLn $ winner ++ " won!"
