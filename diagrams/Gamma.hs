import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)
example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

main = defaultMain $ example # centerXY `atop` rect 12 2 # fc white # centerXY
