import Control.Lens ((??))
import Data.Foldable (foldMap)
import Diagrams.Prelude
import Diagrams.Backend.Cairo

main = renderCairo "pretty.png" (Width 300) $ foldMap (rotateBy ?? square 1 <> hrule 1 <> vrule 1) [0,0.1..0.4]
