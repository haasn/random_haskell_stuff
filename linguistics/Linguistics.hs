import Data.Set (Set)
import qualified  Data.Set as S

type Word     a = [a]
type Alphabet a = Set a
type Language a = Set (Word a)

class Star a b | a -> b where
  star :: a -> b

instance Ord a => Star (Word a) (Language a) where
  star w = S.fromList . map concat $ iterate (w:) []

main = print $ star "hello"
