import Prelude hiding ((>>), undefined)

import GHC.IO (throwIO)
import System.Exit (ExitCode(..))

data Void

undefined :: a
undefined = undefined

ignore :: Functor f => f a -> f Void
ignore = fmap (const undefined)

(>>) :: Monad m => m Void -> m a -> m a
a >> b = a >>= const b

forever :: Monad m => m Void -> m Void
forever a = a >> forever a

putStrLn' :: String -> IO Void
putStrLn' = ignore . putStrLn

exit = undefined

-- New main

main :: IO Void
main = forever $ do
  line <- getLine
  case line of
    "stop" -> exit
    msg    -> putStrLn' msg
