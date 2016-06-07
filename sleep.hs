import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Either (partitionEithers)
import System.Environment (getArgs)
import Text.Printf
import Text.Read (readMaybe)

main :: IO ()
main = do
  input <- getArgs
  case partitionEithers $ map readEither input of
    ([], []) -> putStrLn "sleep: missing operand"
    ([], ts) -> threadDelay $ sum ts * 1000000
    (es,  _) -> forM_ es $ printf "sleep: invalid time interval '%s'\n"

readEither :: Read a => String -> Either String a
readEither s = maybe (Left s) Right $ readMaybe s
