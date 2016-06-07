import Control.Lens
import Data.List.Lens
import Data.Char (toUpper)

-- These should probably be in Control.Lens somewhere, though I haven't checked
-- to see if they violate any lens laws or not
byWords = lens words (const unwords).traverse
byLines = lens lines (const unlines).traverse

main = "file" ^! act readFile.byLines.to capital.act putStrLn
  where capital = byWords._head %~ toUpper
