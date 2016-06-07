{-# LANGUAGE DeriveDataTypeable, GADTs, RankNTypes, TemplateHaskell, TypeFamilies #-}
import Control.Applicative hiding (empty)
import Control.Lens

import Data.Data
import Data.Table hiding (empty)
import Data.Ord (comparing)

_x :: Lens (a,x) (b,x) a b
_x = _1

_y :: Lens (x,a) (x,b) a b
_y = _2

data Wall = Solid | Broken deriving (Eq, Ord, Show, Data, Typeable)
data Mob = Player | Roach deriving (Eq, Ord, Show, Data, Typeable)

data Cell = Cell
  { _coord :: !(Int, Int)
  , _wall  :: !(Maybe Wall)
  , _mob   :: !(Maybe Mob)
  }
  deriving (Eq, Ord, Show, Data, Typeable)

makeLenses ''Cell

instance Tabular Cell where
  type PKT Cell = (Int, Int)
  data Key k Cell a where
    Coord :: Key Primary Cell (Int, Int)
    X     :: Key SupplementalInt Cell Int
    Y     :: Key SupplementalInt Cell Int
    Mob   :: Key Supplemental    Cell (Maybe Mob)
    Wall  :: Key Supplemental   Cell (Maybe Wall)

  data Tab Cell i = CellTab
    (i Primary (Int, Int))
    (i SupplementalInt Int)
    (i SupplementalInt Int)
    (i Supplemental (Maybe Mob))
    (i Supplemental (Maybe Wall))

  fetch Coord = view coord
  fetch X = view (coord._x)
  fetch Y = view (coord._y)
  fetch Mob = view mob
  fetch Wall = view wall

  primary = Coord
  primarily Coord r = r

  mkTab f = CellTab <$> f Coord <*> f X <*> f Y <*> f Mob <*> f Wall
  forTab (CellTab c x y m w) f =
    CellTab <$> f Coord c <*> f X x <*> f Y y <*> f Mob m <*> f Wall w

  ixTab (CellTab c _ _ _ _) Coord = c
  ixTab (CellTab _ x _ _ _) X = x
  ixTab (CellTab _ _ y _ _) Y = y
  ixTab (CellTab _ _ _ m _) Mob = m
  ixTab (CellTab _ _ _ _ w) Wall = w

  autoTab _ = Nothing

empty :: Lens' (Table Cell) (Table Cell)
empty = with Wall (==) Nothing . with Mob (==) Nothing

unit :: Mob -> Lens' (Table Cell) (Table Cell)
unit = with Mob (==) . Just

pos :: (Int, Int) -> Traversal' (Table Cell) Cell
pos c = with Coord (==) c.each

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x,y) (z,w) = max (abs (x-z)) (abs (y-w))

neighbour :: (Int, Int) -> Cell -> Bool
neighbour p c = distance p (c^.coord) <= 1

playerPos :: Table Cell -> (Int, Int)
playerPos = (^?! unit Player . folded . coord)

roach :: (Int, Int) -> Table Cell -> Table Cell
roach old t = case minimumByOf eligible (comparing $ distance player) t of
  Just new -> t & pos new.mob ?~ Roach & pos old.mob .~ Nothing -- update pos
  Nothing  -> t
  where eligible = empty.folded.filtered (neighbour old).coord
        player   = playerPos t
  -- TODO: implement actual roach distance function

test :: Table Cell
test = fromList (liftA2 (\x y -> Cell (x,y) Nothing Nothing) [0..10] [0..10])
  & each.filtered (anyOf (coord.both) (`elem` [0,10])).wall ?~ Solid
  & pos (5,5).mob ?~ Player
  & pos (3,3).mob ?~ Roach
