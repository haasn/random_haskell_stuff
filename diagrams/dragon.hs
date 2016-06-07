import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

dragon n = fromSegments $ iterate f [straight unitX] !! n
  where f s = reverse s ++ rotateBy (-1/4) s

main = defaultMain $ dragon 16 # lw 0.5 # pad 1.1
