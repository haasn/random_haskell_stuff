import Control.DeepSeq
import Control.Monad (void)

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Ptr
-- import Diagrams.Backend.Cairo.List

import qualified Data.ByteString as BS

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)

import System.Posix.IO.ByteString

-- Example diagram

diagram = cylinder 2 # scaleY 0.5 # centerXY # pad 1.1 # lc white

cylinder h = mconcat
  [ unitCircle
  , (origin .+^ unitX ) ~~^ side
  , (origin .+^ unit_X) ~~^ side
  , arc pi (tau :: Rad) # translate side
  ]
  where side = unit_Y # scale h

p ~~^ v = p ~~ (p .+^ v)

-- Render with cairo and write to STDOUT

main :: IO ()
main = do
  let w = 1000; h = 1000

  b <- renderPtr w h diagram
  l1 <- peekArray (w*h*4) b
  free b

  l2 <- foo w h (unitCircle # lc white)

  BS.writeFile "/home/nand/foo1" (BS.pack l1)
  BS.writeFile "/home/nand/foo2" (BS.pack l2)

foo w h d = do
  b <- renderPtr w h d
  l <- peekArray (w*h*4) b
  free b

  return l
