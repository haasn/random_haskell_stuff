import Control.Monad (void, replicateM)

import qualified Data.Map as Map
import           Data.Map (Map)

type Board = Map (Int, Int) Char

example :: Board
example = Map.fromList [((y, x), 'A') | y <- [2..5], x <- [1, 4]]

-- Print a board

printBoard :: Board -> IO ()
printBoard = void . sequence . Map.elems . snd . Map.mapAccumWithKey g (0, 0)
  where g p n c = (n, f p n c)
        f (y,x) (y',x') c = do
          -- Print as many newlines as skipped
          putStr $ replicate (y' - y) '\n'

          -- Print as many whitespace as skipped
          putStr $ replicate (if y' > y then x' else x' - x - 1) ' '

          -- Print the character at that position
          putStr [c]

main = printBoard example
