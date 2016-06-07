data Color = Blue | Green | Red | White | Yellow deriving (Show, Enum, Bounded)
data Nation = Britain | Denmark | Germany | Norway | Sweden deriving (Show, Enum, Bounded)
data Cigar = Blend | Bluemaster | Dunhill | PallMall | Prince deriving (Show, Enum, Bounded)
data Drink = Beer | Coffee | Milk | Tea | Water deriving (Show, Enum, Bounded)
data Pet = Bird | Cat | Dog | Fish | Horse deriving (Show, Enum, Bounded)

type House = (Color, Nation, Cigar, Drink, Pet)
type Solution = (House, House, House, House, House)

universe :: [Solution]
