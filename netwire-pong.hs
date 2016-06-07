{-# LANGUAGE TypeOperators, RankNTypes, TemplateHaskell, Arrows, LambdaCase
  , NoMonomorphismRestriction #-}

import Prelude hiding ((.), id)

import Control.Lens
import Control.Monad.Identity
import Control.Wire hiding (Event)

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S (empty)

import Graphics.Gloss.Interface.Pure.Game

-- Constants

pWidth  = 0.02
pHeight = 0.3
bRadius = 0.05
fWidth  = 2
fHeight = 1
tSize   = 0.0005

-- ObjectState lenses
makeLensesFor [("objVelocity","objVel"),("objPosition","objPos")] ''ObjectState

type (~>)  = WireP
type Vec   = (Double, Double)
type Input = Set Key

type BallState   = ObjectState Vec
type PaddleState = Double
type Score       = (Int, Int)

-- Paddles
paddle :: Input ~> PaddleState
paddle = proc i -> do
  let (u, d) = ( i^.contains (SpecialKey KeyUp  )
               , i^.contains (SpecialKey KeyDown))
  integralLim_ limitPaddle 0 -< (fromIntegral $ fromEnum u - fromEnum d, ())

limitPaddle :: a -> b -> PaddleState -> PaddleState
limitPaddle _ _ = fwhen ((>pMax) . view val) $ val .~ pMax
  where pMax = fHeight/2 - pHeight/2

-- Ball
initialBall :: BallState
initialBall = ObjectState (0,0) (2,1)

data PointE = LeftE | RightE

ball :: (Input, PaddleState) ~> (BallState, Maybe PointE)
ball = proc (i, p) -> do
  let (d, e) = if i ^. contains (SpecialKey KeySpace)
        then (Position   (0,0), Just LeftE)
        else (Accelerate (0,0), Nothing   )
  s  <- object_ collideBall initialBall -< (d, p)
  id -< (s, e)

-- For now, just reverse the velocity when exceeding magnitude 1 on the x axis
-- I have no idea how to handle collisions and scoring and stuff nicely
collideBall :: PaddleState -> BallState -> BallState
collideBall _ = fwhen ((>bMax) . view (objPos._1.val)) $ do
  s <- view (objPos._1.sign.to negate)
  objVel.both.sign .~ s
 where bMax = fWidth/2 - bRadius

-- Score bookkeeping
score :: Maybe PointE ~> Score
score = (`accum` (0,0)) . flip $ \case
  Just LeftE  -> _1 +~ 1
  Just RightE -> _2 +~ 1
  Nothing     -> id

-- Game loop
game :: Maybe Event ~> Picture
game = proc e -> do
  i <- input  -< e -- Keep track of input in a special set
  p <- paddle -< i
  (b, e) <- ball -< (i, p)
  s <- score -< e

  display -< (p, b, s)

display :: (PaddleState, BallState, Score) ~> Picture
display  = arr $ \(p, b, s) -> pictures
  [ circleSolid bRadius `at` b^.objPos
  , rectangleSolid pWidth pHeight `at`(-1 - pWidth/2, p)
  , rectangleWire fWidth fHeight
  , drawScore s `at` (-0.1, 0.35)
  ]
 where p `at` (x,y) = translate (realToFrac x) (realToFrac y) p; infixr 1 `at`

drawScore :: Score -> Picture
drawScore (x, y) = scale tSize tSize . text $ show x ++ " " ++ show y

-- Input handling
input :: Maybe Event ~> Input
input = (`accum` S.empty) . flip $ \case
  Just (EventKey k m _ _) -> contains k .~ (m == Down)
  _ -> id

-- Main
main :: IO ()
main = play window white 100 game draw event step
 where
  window = InWindow "pong" (1000,400) (0,0)
  draw    w = either (error . show) (scale 400 400) $
                stepWireP w 0 Nothing ^. _1
  event e w = stepWireP w 0 (Just e) ^. _2
  step  t w = stepWireP w (realToFrac t) Nothing & view _2

-- Functional when
fwhen :: (a -> Bool) -> (a -> a) -> a -> a
fwhen c f x
  | c x = f x
  | otherwise = x

-- Some numerical lenses I made up
-- NOTE: I later realized these were illegal when dealing with 0.
val, sign :: Num a => Lens' a a
val  = lens abs (\a v -> signum a * abs v)
sign = lens signum (\n s -> signum s * abs n)

-- Shouldn't this be some combinator in Control.Wire?
maybeW :: (Monoid e, Monad m) => Wire e m a (Maybe b) -> Wire e m a b
maybeW = (.) . mkFix . const $ maybe (Left mempty) Right
