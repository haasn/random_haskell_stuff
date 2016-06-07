import Control.Lens

data Numeral = I | V | X | L | C | D | M
  deriving (Show, Read, Ord, Enum)

values :: [(Numeral, Integer)]
values = zip [I .. M] [1,5,10,50,100,500,1000]
