{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (log)
import Control.Monad.Free

data LogF a = LogF { log :: String, act :: IO a } deriving Functor
type Log = Free LogF

test :: Log ()
test = liftF $ LogF "logging" (putStrLn "acting")

interactLog :: Log a -> IO a
interactLog (Pure x) = return x
interactLog (Free l) = do
  putStrLn $ "[Log] " ++ log l
  act l >>= interactLog
