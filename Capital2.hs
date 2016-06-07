import Data.Char (toUpper, isSpace)

main = readFile "file" >>= f toUpper
 where
  f g []     = return ()
  f g (x:xs) = putChar (g x) >> if isSpace x then f toUpper xs else f id xs
