import Data.List (transpose)

textBox :: String -> Int -> String -> String
textBox border maxWidth str = unlines $ [ header ] ++ content ++ [ header ]
    where header   = take boxWidth $ cycle border
          content  = map (pad . align) lines
          boxWidth = maximum $ map length content

          pad line  = unwords $ [ border, line, border ]
          lines     = reverse . map reverse . foldl f [] $ words str
          textWidth = maximum $ map (length . unwords) lines

          align line = concat . concat $ transpose [line, spaces]
            where lineLength    = sum $ map length line
                  numSpaces     = length line - 1
                  spaces        = [ replicate (if i < r then q+1 else q) ' '
                                  | i <- [0 .. numSpaces - 1] ]
                  (q, r)        = (textWidth - lineLength) `quotRem` numSpaces

          f []     w = [[w]]
          f (l:ls) w
            | length (pad $ unwords l') <= maxWidth = l' : ls
            | otherwise = [w] : l : ls
            where l' = w : l
