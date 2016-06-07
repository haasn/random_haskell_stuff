import Data.Monoid (Monoid, mconcat)
import Text.Parsec
import Text.Parsec.String

whileBalanced :: Parser String
whileBalanced = collect [blob, option [] center, blob]
 where
  blob   = many (noneOf "()")
  center = collect [string "(", whileBalanced, string ")"]

collect :: Monoid a => [Parser a] -> Parser a
collect = fmap mconcat . sequence
