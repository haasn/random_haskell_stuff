import Control.Applicative
import Control.Arrow (second)
import Control.Monad (guard)
import "mtl" Control.Monad.Writer

import Data.List (find, groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

type VVar = String
type TVar = Integer

type VContext = [(VVar, Type)]
type TContext = [(TVar, Type)]

data Type
  = UnitT
  | AbsT Type Type
  | PairT Type Type
  | VarT TVar
  deriving Eq

-- Pretty printer

instance Show Type where
  show UnitT       = "1"
  show (AbsT a b)  = "(" ++ show a ++ " → " ++ show b ++ ")"
  show (PairT a b) = show a ++ "×" ++ show b
  show (VarT n)    = 't' : show n

data Lambda
  = Unit
  | Free VVar
  | Abs VVar Type Lambda
  | App Lambda Lambda
  | Pair Lambda Lambda
  | Fst Lambda
  | Snd Lambda
  deriving Show

-- | Attempt to evaluate a lambda expression's type

getType :: VContext -> Lambda -> Maybe Type
getType _    Unit       = return UnitT
getType ctx (Free n)    = lookup n ctx
getType ctx (Abs n t l) = AbsT t <$> getType ((n, t) : ctx) l

getType ctx (App l1 l2) = do
  AbsT t1 tr <- getType ctx l1
  let l2'     = makeUnique t1 l2
  t2         <- getType ctx l2'

  tctx <- snd <$> runWriterT (unify t1 t2)

  -- sanity assertion
  AbsT t1' tr' <- getType ctx (rewrite tctx l1)
  t2'          <- getType ctx (rewrite tctx l2')

  unless (t1' == t2') . error $
    "Type mismatch: " ++ show t1' ++ " ~ " ++ show t2'

  return tr'

getType ctx (Pair a b) = PairT <$> getType ctx a <*> getType ctx b

getType ctx (Fst l) = do
  PairT t1 _ <- getType ctx l
  return t1

getType ctx (Snd l) = do
  PairT _ t2 <- getType ctx l
  return t2

-- | Rewrite a Lambda expression as per the rules of a type context

rewrite :: TContext -> Lambda -> Lambda
rewrite _   Unit       = Unit
rewrite _   l@(Free _) = l

rewrite ctx (Abs v t l) = Abs v (go t) (rewrite ctx l)
  where
    go UnitT = UnitT
    go t@(VarT n) = fromMaybe t (lookup n ctx)
    go (AbsT t1 t2) = AbsT (go t1) (go t2)
    go (PairT t1 t2) = PairT (go t1) (go t2)

rewrite ctx (App l1 l2)  = App (rewrite ctx l1) (rewrite ctx l2)
rewrite ctx (Pair l1 l2) = Pair (rewrite ctx l1) (rewrite ctx l2)

rewrite ctx (Fst l) = Fst (rewrite ctx l)
rewrite ctx (Snd l) = Snd (rewrite ctx l)

-- | Unify two types

type Unify = WriterT TContext Maybe

unify :: Type -> Type -> Unify Type
unify t1 t2 = case runWriterT (unify' t1 t2) of
  Nothing    -> error $ "unify' failed: " ++ show t1 ++ " ~ " ++ show t2
  Just (t,c) -> maybe (error "smash failed") tell (smash c) >> return t

-- This operation is more or less* commutative so I'll remove one type at each
-- step then provide a catch-all which swaps the parameters
--
-- * The rewrite rules will be in the opposite order but the result is the same
unify' :: Type -> Type -> Unify Type

-- A variable can always be specialized
unify' t@(VarT v) (VarT v')
  | v == v' = return t -- Avoid carrying around unnecessary contexts
unify' (VarT v) t = tell [(v, t)] >> return t

-- A unit only matches a unit
unify' UnitT UnitT       = return UnitT
unify' UnitT (AbsT _ _)  = error "UnitT ~ AbsT"
unify' UnitT (PairT _ _) = error "UnitT ~ PairT"

unify' (AbsT a1 b1) (AbsT a2 b2) = do
  a <- unify' a1 a2
  b <- unify' b1 b2
  return $ AbsT a b
unify' (AbsT _ _) (PairT _ _) = error "AbsT ~ PairT"

unify' (PairT a1 b1) (PairT a2 b2) = do
  a <- unify' a1 a2
  b <- unify' b1 b2
  return $ PairT a b

-- The commutative catch-all
unify' a b = unify' b a

-- Given a list of equality constraints, ‘smash’ the variables together.
-- Can fail, eg. if provided conflicting constraints
smash :: TContext -> Maybe TContext
smash ctx = fmap (uncurry (++)) . runWriterT $ mapM (foldM1 contract) groups
  where
    -- Groups of all identical variables, to eliminate rewrite rules
    groups = groupBy (equating fst) ctx

    -- Unify two var/type pairs
    contract (v, t1) (_, t2) = (,) v <$> unify' t1 t2

-- Using a reference type, make all variables in a second type unique with
-- respect to the reference, but while preserving identicalness.
makeUnique :: Type -> Lambda -> Lambda
makeUnique ref = goL
  where
    start = case freeTVars ref of
      [] -> 0
      xs -> maximum xs + 1
    goT UnitT = UnitT
    goT (AbsT t1 t2)  = AbsT (goT t1) (goT t2)
    goT (PairT t1 t2) = PairT (goT t1) (goT t2)
    goT (VarT t) = VarT (t + start)

    goL Unit = Unit
    goL v@(Free _) = v
    goL (Abs v t l) = Abs v (goT t) (goL l)
    goL (App l1 l2) = App (goL l1) (goL l2)
    goL (Pair l1 l2) = Pair (goL l1) (goL l2)
    goL (Fst l) = Fst (goL l)
    goL (Snd l) = Snd (goL l)

-- Collect all free type variables in a type, used for makeUnique
freeTVars :: Type -> [TVar]
freeTVars UnitT = []
freeTVars (AbsT t1 t2)  = freeTVars t1 ++ freeTVars t2
freeTVars (PairT t1 t2) = freeTVars t2 ++ freeTVars t2
freeTVars (VarT t) = [t]

-- Small helper, like comparing but for (==)
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating = on (==)

-- Mixture between foldM and foldl1
foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "foldM1: empty list"
foldM1 f (x:xs) = foldM f x xs

-- Examples, inferred types shown (with superfluous parentheses removed)

id', app', s, k, pair, fst', snd', comp :: Lambda

-- t0 → t0
id' = Abs "x" (VarT 0) (Free "x")

-- (t0 → t1) → t0 → t1
app' =
  Abs "f" (AbsT (VarT 0) (VarT 1))
    (Abs "x" (VarT 0)
      (App (Free "f") (Free "x")))

-- (t0 → t1 → t2) → (t0 → t1) → t0 → t2
s =
  Abs "x" (AbsT (VarT 0) (AbsT (VarT 1) (VarT 2)))
    (Abs "y" (AbsT (VarT 0) (VarT 1))
      (Abs "z" (VarT 0)
        (App
          (App (Free "x") (Free "z"))
          (App (Free "y") (Free "z")))))

-- t0 → t1 → t0
k =
  Abs "x" (VarT 0)
    (Abs "y" (VarT 1)
      (Free "x"))

-- (t0 → t1) → (t2 → t3) → t0×t2 → t1×t3
pair =
  Abs "f" (AbsT (VarT 0) (VarT 1))
    (Abs "g" (AbsT (VarT 2) (VarT 3))
      (Abs "p" (PairT (VarT 0) (VarT 2))
        (Pair
          (App (Free "f") (Fst (Free "p")))
          (App (Free "g") (Snd (Free "p"))))))

-- t0×t1 → t0
fst' =
  Abs "x" (PairT (VarT 0) (VarT 1))
    (Fst (Free "x"))

-- t0×t1 → t1
snd' =
  Abs "x" (PairT (VarT 0) (VarT 1))
    (Snd (Free "x"))

-- (t1 → t2) → (t0 → t1) → t0 → t2
comp =
  Abs "f" (AbsT (VarT 1) (VarT 2))
    (Abs "g" (AbsT (VarT 0) (VarT 1))
      (Abs "x" (VarT 0)
        (App (Free "f")
          (App (Free "g") (Free "x")))))
