import System.Environment (getArgs)
import System.Random (randomRIO)

choose :: [a] -> IO a
choose xs = fmap (xs!!) $ randomRIO (0, length xs - 1)

answer :: IO String
answer = choose
  [ "That's a nice number."
  , "Really, that's your favorite number?"
  , "I don't like that number."
  , "Awesome number!"
  , "Not what I would have picked."
  , "Cool number!"
  , "I love that number!"
  , "That's ok, I guess."
  , "Good pick!"
  , "Oh well"
  ]

main = do
  n:_ <- getArgs
  putStrLn $ "So your lucky number is " ++ n ++ "?"
  answer >>= putStrLn
