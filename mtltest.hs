import Control.Monad.State

test :: (Num n) => StateT n IO ()
test = do
  n <- get
  liftIO $ print n
  put (n + 1)

main = runStateT test 5
