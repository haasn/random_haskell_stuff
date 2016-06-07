import Diagrams.Prelude
import Diagrams.Backend.Cairo

main = renderCairo "circle.png" (Width 550) $ unitCircle # pad 1.1
