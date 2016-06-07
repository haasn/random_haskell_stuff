{-# LANGUAGE GADTs, ScopedTypeVariables, ViewPatterns #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (op)
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Tree.Lens
import Data.Tree.Pretty

import System.Environment (getArgs)
import Text.Read (readMaybe)

type Atom      = String
type Value     = (Double, Tree String)
type Stack     = [Value]
type Operation = Stack -> Either (Tree String) Stack

main :: IO ()
main = mapM_ putStrLn . parse . words . unwords =<< getArgs

parse :: [Atom] -> [String]
parse [] = usage
parse xs = case runWriter (traverse atom xs) & _1 %~ sequence of
  (Just ys, _) -> solve ys
  (Nothing, h) -> "Parse error:" : align (unwords xs) (unwords h)
  where align = concatMap (toListOf both) .: zip `on` chunksOf 80

solve :: [Operation] -> [String]
solve = either stackError (map (show . fst)) . foldl' (>>=) (return [])
  where stackError t = ["Stack error:", drawVerticalTree t]

atom :: Atom -> Writer [String] (Maybe Operation)
atom x = let o = lit x <|> fun x in o <$ tell [c o <$ x]
  where c = maybe '^' (const ' ')

lit, fun :: Atom -> Maybe Operation
lit s = flip op s <$> (readMaybe s :: Maybe Double)
fun   = lookup ?? funs

funs :: [(String, Operation)]
funs = let n ~> f = (n, f $ "(" ++ n ++ ")") in
  [ "+"   ~> op (+)
  , "-"   ~> op (-)
  , "*"   ~> op (*)
  , "/"   ~> op (/)
  , "^"   ~> op (**)
  , "!"   ~> op (product . enumFromTo 1)
  , "pi"  ~> op (pi :: Double)
  , "sum" ~> opn sum
  ]

class Op t where
  op :: t -> String -> Operation
  -- Needed to access the correct position in the stack
  arity :: t -> Int

instance Op Double where
  op d n xs = Right $ (d, Node n []) : xs
  arity _ = 0

instance (d ~ Double, Op t) => Op (d -> t) where
  -- Grab out the nth position of the stack instead of the topmost
  -- to ensure correct argument order
  op f n s = case splitAt (arity f - 1) s of
    (xs, []) -> Left . Node n $ map snd xs ++ [Node "?" []]
    (xs, (v,t):ys) ->
      op (f v) n (xs++ys) & choosing id (_head._2).branches %~ (t:)

  arity f = 1 + arity (f undefined)

opn :: ([Double] -> Double) -> String -> Operation
opn f n (reverse -> xs) = Right [(f $ map fst xs, Node n $ map snd xs)]

usage :: [String]
usage =
  [ "Example:"
  , "./clac \"2 2 + 5 7 * /\""
  ]
