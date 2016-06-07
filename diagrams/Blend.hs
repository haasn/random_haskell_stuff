import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

diagram = square 1 # fcA (red  `withOpacity` 0.7)
   `atop` square 1 # fcA (blue `withOpacity` 0.7) # translate (r2 (0.5, 0))

main = defaultMain $ diagram # lw 0 # centerXY `atop` rect 1.5 1 # fc white
