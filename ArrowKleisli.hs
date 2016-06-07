{-# LANGUAGE Arrows #-}

import Data.Char (toUpper)
import Control.Arrow

foo :: IO ()
foo = do
  x <- getLine
  y <- getLine

  putStrLn y
  putStrLn (map toUpper x)

-- Kleislified getLine/putStrLn
getLineA :: Kleisli IO a String
getLineA = Kleisli (const getLine)

putStrLnA :: Kleisli IO String ()
putStrLnA = Kleisli putStrLn

bar :: Kleisli IO a ()
bar = proc _ -> do
  x <- getLineA -< ()
  y <- getLineA -< ()

  putStrLnA -< y
  putStrLnA -< map toUpper x
