import Control.Lens ((%))
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain $ maze 100 0 0.1 # lw 0.02 # lc white # pad 1.1

mazeRing w = arc (Rad w) (tau :: Rad) <> p ~~ (p .+^ unit_X # scaleX w)
  where p = p2 (1, 0)

maze n r w = mconcat . take n $ iterate step (mazeRing w)
  where step = rotateBy r . reflectY . scale (1-w)
