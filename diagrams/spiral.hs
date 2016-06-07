import Control.Lens ((??))
import Data.Foldable (foldMap)
import Diagrams.Prelude
import Diagrams.Backend.Cairo

main = renderCairo "spiral.png" (Width 1000) . pad 1.1 . centerY . lw 0.005 $
         (spiral 90 ||| strutX 0.2 ||| spiral 89)
           === strutY 0.2 ===
         (spiral 72 ||| strutX 0.2 ||| spiral 45)

spiral d = center . sized (Width 1) . fromSegments . zipWith scale [1,0.99..0.01] $
            iterate (rotate $ d @@ deg) (straight unit_Y)
