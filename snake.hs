{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

import Control.Applicative
import Control.Conditional (whenM)
import Control.Lens hiding (_head, _tail)
import Control.Monad
import Control.Monad.State

import Data.Ix (inRange)
import Data.List (transpose)
import Data.NumInstances.Tuple
import qualified Data.Sequence as S

import System.Random
import Text.PrettyPrint
import Prelude hiding (head, tail)

type Point = (Int, Int)
data Dir = N | E | S | W deriving Show

instance (Random a, Random b) => Random (a, b) where
  randomR ((a,b),(c,d)) g = ((x,y), g'')
    where (x,g' ) = randomR (a,c) g; (y,g'') = randomR (b,d) g'

  random g = ((x,y), g'') where (x,g') = random g; (y,g'') = random g'

data Snake = Snake
  { head :: Point
  , tail :: S.Seq Point
  , size :: Int
  } deriving Show
makeLensesWith (lensRules & lensField .~ Just . ('_':)) ''Snake

data Game = Game
  { _food  :: Point
  , _dir   :: Dir
  , _snake :: Snake
  , _rng   :: StdGen
  } deriving Show
makeLenses ''Game

bounds :: (Point, Point)
bounds = ((0,0), (40,20))

initial :: IO Game
initial = randomR bounds <$> newStdGen <&> \(p,r) ->
  Game p E (Snake (20,10) S.empty 3) r

dirVec :: Dir -> Point
dirVec = \case N -> (0,-1); E -> (1,0); S -> (0,1); W -> (-1,0)

move :: Dir -> Snake -> Maybe Snake
move d s@Snake{..} = do
  let new = head + dirVec d
      f | S.length tail < size = id -- Don't drop the tail while growing
        | otherwise            = view _init
  guard (inRange bounds new && notElemOf folded new tail)
  return s { head = new, tail = f $ head <| tail }

step :: Game -> Maybe Game
step = execStateT $ do
  d <- use dir
  snake <~ (lift . move d =<< use snake)
  whenM ((==) <$> use (snake._head) <*> use food) $ do
    snake._size += 1
    food <~ zoom rng (state $ randomR bounds)

pretty :: Game -> String
pretty Game{_food, _snake = Snake{head,tail}} =
  unlines . wrap (w+2) . transpose . wrap h $
    foldrOf folded (\n -> at n .~ '·') empty tail
      & at _food .~ '*' & at head .~ '@'
  where empty = replicate w (replicate h ' ')
        at n  = (traversed<.>traversed).index n
        wrap h xs = let b = replicate h '#' in b : xs ++ [b]
        ((0,0), (w,h)) = bounds & _2.both +~ 1
