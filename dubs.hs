import Control.Lens
import Control.Applicative
import Data.Monoid (mappend)
import Data.List (group)
import FourChan
import System.Environment (getArgs)

main = do
  [s] <- getArgs
  ps  <- getPosts <$> getThread "g" (read s)
  traverseOf_ (folded.to getPostId.to dubs._Just) putStrLn ps

dubs :: Int -> Maybe String
dubs n = mappend (">>" ++ show n ++ " ") <$> lookup digits words
  where words  = zip [2..] ["dubs", "trips", "quads"]
        digits = length . last . group $ show n
