{-
-   Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
- 
-   clac is free software: you can redistribute it and/or modify
-   it under the terms of the GNU General Public License as published by
-   the Free Software Foundation, either version 3 of the License, or
-   (at your option) any later version.

-   clac is distributed in the hope that it will be useful,
-   but WITHOUT ANY WARRANTY; without even the implied warranty of
-   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-   GNU General Public License for more details.

-   You should have received a copy of the GNU General Public License
-   along with clac  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Applicative
import Data.Maybe
import System.Environment
import Text.Read
import Data.List (foldl')


type Stack = [Double]
type Equation = [String]
type Solution = String
type Operator = String


main ::  IO ()
main = do equation <- parseArgs
          case equation of
            [] -> printHelp
            _ -> putStrLn $ solve equation

parseArgs ::  IO Equation
parseArgs = (>>= words) <$> getArgs

printHelp :: IO ()
printHelp = putStrLn (unlines help)

help :: [String]
help = ["clac is an RPN (postfix) calculator:"
       ,"    <https://en.wikipedia.org/wiki/Reverse_Polish_notation>."
       ,""
       ,"clac supports the following operations:"
       ,""] ++ (map (\(op, (desc, _)) -> op ++ "\t" ++ desc) ops)

solve ::  Equation -> Solution
solve = unwords . map show . foldl' (flip f) []
        where f :: String -> Stack -> Stack
              f op = fromMaybe (error $ "Unrecognised operator: " ++ op) $ lookup3 op ops <|> numberP op

lookup2 ::  (Eq a) => a -> [(a,(b,c))] -> Maybe b
lookup2 _ [] =  Nothing
lookup2 key ((x,(y,_)):xys)
  | key == x  =  Just y
  | otherwise =  lookup2 key xys

lookup3 ::  (Eq a) => a -> [(a,(b,c))] -> Maybe c
lookup3 _ [] =  Nothing
lookup3 key ((x,(_,z)):xys)
  | key == x  =  Just z
  | otherwise =  lookup3 key xys

numberP :: String -> Maybe (Stack -> Stack)
numberP = fmap (:) . readMaybe

ops ::  [(Operator, (String, Stack -> Stack))]
ops = 
  [("*"   , ("multiply two numbers",                                 binOp  (*)))
 ,("+"   , ("add two numbers",                                       binOp  (+)))
 ,("-"   , ("subtract one number from another",                      binOp  (-)))
 ,("/"   , ("divide one number with another",                        binOp  (/)))
 ,("**"  , ("raise one number to the power of another",              binOp  (**)))
 ,("^"   , ("raise one number to the power of an integer",           binOp  (\x y -> fromInteger ((^) (floor y)
                                                                                                      (floor x :: Integer)))))
 ,("mod" , ("get the remainder of dividing one number with another", binOp  (\x y -> fromInteger (mod (floor y) (floor x)))))
 ,("div" , ("divide one number with an integer",                     binOp  (\x y -> fromInteger (div (floor y) (floor x)))))
 ,("~"   , ("negate a number",                                       unOp   negate))
 ,("abs" , ("get the absolute value of a number",                    unOp   abs))
 ,("cos" , ("get the cosine of a number",                            unOp   cos))
 ,("sin" , ("get the sine of a number",                              unOp   sin))
 ,("tan" , ("get the tangent of a number",                           unOp   tan))
 ,("atan", ("get the arctangent of a number",                        unOp   atan))
 ,("ln"  , ("get the natural logarithm of a number",                 unOp   log))
 ,("sum" , ("sum all previous numbers",                              naryOp sum))
 ,("pi"  , ("get pi",                                                (:)    (pi :: Double)))
 ]

binOp ::  (a -> a -> a) -> [a] -> [a]
binOp (f) (x:y:rest) = f y x : rest

unOp ::  (a -> a) -> [a] -> [a]
unOp f (x:rest) = f x:rest

naryOp ::  ([a] -> a) -> [a] -> [a]
naryOp f xs = [f xs]
