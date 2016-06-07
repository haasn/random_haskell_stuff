import Control.Lens ((%))
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main = defaultMain $ diagram # lw 0.02 # lc white

diagram = cylinder 2 # scaleY 0.5 # centerXY # pad 1.1

cylinder h = mconcat
  [ unitCircle
  , (origin .+^ unitX ) ~~^ side
  , (origin .+^ unit_X) ~~^ side
  , arc pi (tau :: Rad) # translate side
  ]
  where side = unit_Y # scale h

p ~~^ v = p ~~ (p .+^ v)
