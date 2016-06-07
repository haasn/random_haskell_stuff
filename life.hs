{-# LANGUAGE DeriveFunctor, TemplateHaskell, RankNTypes, NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Comonad
import Control.Lens
import qualified Data.List.Zipper as LZ

-- Helper: Generalization of list zippers to a two-dimensional zipper plane
newtype Z a = Z (LZ.Zipper (LZ.Zipper a)) deriving (Show, Functor, Eq)
makePrisms ''Z

instance Comonad Z where
    extract   = LZ.extractz . LZ.extractz . view _Z
    duplicate = over _Z $ LZ.duplicatez . fmap (review _Z . LZ.duplicatez)

right, left, up, down :: Iso' (Z a) (Z a)
right = wrap _Z         $ iso LZ.right LZ.left
up    = wrap _Z.mapping $ iso LZ.right LZ.left
left = from right
down = from up

-- 'Wraps' an automorphic lenslike in an isomorphism
wrap p i = cloneIso p . i . from p

focus :: Lens' (Z a) a
focus = _Z.cursor.cursor
    where cursor f lz = f (LZ.cursor lz) <&> \x -> LZ.replace x lz

-- Actual game of life
neighbours = mconcat cells . focus
    where cells = horiz ++ vert ++ liftA2 (.) horiz vert
          horiz = [right, left]
          vert  = [up, down]

rule :: Z Bool -> Bool
rule z = case lengthOf (neighbours.only True) z of
    2 -> extract z
    3 -> True
    _ -> False

step :: Z Bool -> Z Bool
step = extend rule
