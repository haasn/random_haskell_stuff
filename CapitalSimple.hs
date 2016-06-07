module Main where
import Data.Char

convert :: String -> String
convert = unlines . (map convertLine) . lines

convertLine :: String -> String
convertLine = unwords . (map convertWord) . words

convertWord :: String -> String
convertWord s = (toUpper (head s)):(tail s)

main = do
    name <- readFile "file"
    putStr $ convert name
