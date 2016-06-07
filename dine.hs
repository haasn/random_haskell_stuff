import Control.Monad (void, forever)
import Data.Foldable (foldrM)
import Control.Concurrent
import Control.Concurrent.STM

type Fork = TMVar ()
type Philosopher = Fork -> Fork -> IO ()

newFork = newTMVarIO ()
getFork = takeTMVar
putFork = putTMVar `flip` ()

philosopher :: String -> Philosopher
philosopher name left right = void . forkIO . forever $ do
  atomically (getFork left >> getFork right)
  putStrLn $ name ++ " is eating."
  threadDelay 1000000
  putStrLn $ name ++ " is done eating."
  atomically (putFork left >> putFork right)
  threadDelay 500000

dine :: [Philosopher] -> IO ()
dine xs@(x:_)   = do f <- newFork; p <- foldrM go (x f) xs; p f
  where go p p' = do f <- newFork; p' f; return (p f)

main = dine [ philosopher "Baruch Spinoza"
            , philosopher "Gilles Deleuze"
            , philosopher "Karl Marx"
            , philosopher "Friedrich Nietzsche"
            , philosopher "Michel Foucault"
            ]
