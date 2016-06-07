import Control.Monad (forever)

class Joke j where
  tell :: j -> IO ()

data Funny = FunnyJoke

instance Joke Funny where
  tell FunnyJoke = do
    putStrLn "Haha this is a joke"
    putStrLn "so funny"


main :: IO ()
main = forever $ tell FunnyJoke
