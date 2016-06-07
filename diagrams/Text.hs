import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain $ text "Hello, world!" # scale 0.01 `atop` rect 3 1 # fc white
