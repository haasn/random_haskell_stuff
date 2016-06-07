import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Control.Monad.Trans.State (state)
import Control.Monad.Reader

import Data.Map (Map)
import Debug.Trace (trace)

import qualified Data.Map as M

-- Some examples for debugging

id', s, k, ap', comp :: Lambda

-- t0 → t0
id' = Abs "x" (Free "x")

-- t0 → t1 → t0
k   = Abs "x" (Abs "y" (Free "x"))

-- (t2 → t6 → t8) → (t2 → t6) → t2 → t8
s   = Abs "x" (Abs "y" (Abs "z" (App (App (Free "x") (Free "z"))
                                     (App (Free "y") (Free "z")))))

-- (t1 → t3) → t1 → t3
ap' = Abs "f" (Abs "x" (App (Free "f") (Free "x")))

-- (t4 → t6) → (t2 → t4) → t2 → t6
comp = Abs "f" (Abs "g" (Abs "x" (App (Free "f") (App (Free "g") (Free "x")))))

-- Church numerals

zero, one, two :: Lambda
zero = Abs "f" (Abs "x" (Free "x"))
one  = Abs "f" (Abs "x" (App (Free "f") (Free "x")))
two  = Abs "f" (Abs "x" (App (Free "f") (App (Free "f") (Free "x"))))

debug :: Monad m => String -> m ()
debug s = trace s $ return ()

-- Variable types
type VVar = String
type TVar = Integer

-- Rewrite rules for type variables
type Ruleset = Map TVar Type

-- Bound variables
type Binding  = (VVar, Type)

data Context = Context
  { bindings :: [Binding]
  , unique   :: TVar
  }

data Type
  = UnitT
  | VarT TVar
  | AbsT Type Type
  deriving Eq

instance Show Type where
  show UnitT        = "1"
  show (VarT t)     = 't' : show t
  show (AbsT t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")"

data Lambda
  = Unit
  | Free VVar
  | Abs VVar Lambda
  | App Lambda Lambda

instance Show Lambda where
  show Unit        = "1"
  show (Free v)    = v
  show (Abs v l)   = "λ" ++ v ++ ". " ++ show l
  show (App l1 l2) = '(' : show l1 ++ " " ++ show l2 ++ ")"

-- Determine the Type of a Lambda
typeOf :: Lambda -> Ctx Type
typeOf Unit     = return UnitT
typeOf (Free v) = gets bindings >>= maybe (left (FreeError v)) return . lookup v

typeOf (Abs s l) = do
  trace ("[Abs] Inserting: " ++ s) $ insert s
  tr <- trace ("[Abs] Getting typeOf l: " ++ show l) $ typeOf l
  debug $ "[Abs] typeOf l [ " ++ show l ++ " ] was: " ++ show tr
  AbsT <$> trace ("[Abs] Extracting " ++ s) (extract s) <*> pure tr

typeOf (App l1 l2) = do
  t1 <- trace ("[App] Getting typeOf l1: " ++ show l1) $ typeOf l1
  debug $ "[App] typeOf l1 [ " ++ show l1 ++ " ] was: " ++ show t1
  t2 <- trace ("[App] Getting typeOf l2: " ++ show l2) $ typeOf l2

  -- Make the types unique and add a rewrite rule
  t2 <- apply . unify t2 =<< makeUnique t2
  debug $ "[App] typeOf l2 [ " ++ show l2 ++ " ] was: " ++ show t2

  v <- newTVar
  let abst = AbsT t2 (VarT v)
  debug $ "Unifying: " ++ show t1 ++ " ~ " ++ show abst
  AbsT t2 tr <- apply $ t1 `unify` abst
  debug $ "Unified type was: " ++ show (AbsT t2 tr)

  return tr

-- Unification of two types
unify :: Type -> Type -> Rules Type

-- Variables can always be specialized
unify t@(VarT v) (VarT  v')
  | v == v = return t
unify (VarT v) t = rule v t >> return t

-- The rest only match their respective counterparts
unify UnitT UnitT               = return UnitT
unify UnitT t@(AbsT _ _)        = left (UnifyError UnitT t)
unify (AbsT a1 b1) (AbsT a2 b2) = AbsT <$> unify a1 a2 <*> unify b1 b2

-- Unification order doesn't matter
unify a b = unify b a

-- Add a rewrite rule
rule :: TVar -> Type -> Rules ()
rule v t = do
  when (v `occurs` t) $ left (InfiniteType v t)
  rules <- get
  case M.lookup v rules of
    Nothing -> put (M.insert v t rules)
    Just t' -> do
      tnew <- unify t t'
      put (M.insert v tnew rules)

-- Check if a variable occurs in a type
occurs :: TVar -> Type -> Bool
occurs _ UnitT        = False
occurs v (VarT v')    = v == v'
occurs v (AbsT t1 t2) = occurs v t1 || occurs v t2

-- Apply the results of a Rules computation to a Ctx Type
apply :: Rules Type -> Ctx Type
apply x = case runStateT x M.empty of
  Left e       -> left e
  Right (a, r) -> go >> modifyBinds (update r) >> trace ("[apply] rules: " ++ show r) (return (rewrite r a))
    where go = do { s <- gets bindings; debug ("[apply] old binds: " ++ show s) }

-- Update a Bindings by rewriting all VarTs
update :: Ruleset -> [Binding] -> [Binding]
update _ []          = []
update r ((v, t):xs) = (trace ("Rewriting " ++ v ++ ": " ++ show t ++ " ⇒ " ++ show t') v, t') : update r xs
  where t' = rewrite r t

-- Rewrite a Type according to a certain Ruleset
rewrite :: Ruleset -> Type -> Type
rewrite _ UnitT = UnitT
rewrite r t@(VarT v) = M.findWithDefault t v r
rewrite r (AbsT t1 t2) = AbsT (rewrite r t1) (rewrite r t2)

-- Extract a variable's type from the context
extract :: VVar -> Ctx Type
extract s = do
  ctx <- get
  let (t, s') = go (bindings ctx)
  put ctx { bindings = s' }
  return t

  where
    -- This should never occur normally
    go [] = error $ "Unable to extract variable from context:" ++ s
    go (v@(s', t):vs)
      | s == s'   = (t, vs)
      | otherwise = second (v:) (go vs)

-- Inserts a variable under a fresh TVar
insert :: VVar -> Ctx ()
insert s = do
  var <- newTVar
  modifyBinds ((s, VarT var):)

-- Make fresh type vars
newTVar :: Ctx TVar
newTVar = do
  ctx <- get
  let next = unique ctx
  put ctx { unique = next + 1 }
  return next

makeUnique :: Type -> Ctx Type
makeUnique t = (`offset` t) <$> newTVar

offset :: TVar -> Type -> Type
offset _ UnitT        = UnitT
offset n (VarT t)     = VarT (n+t)
offset n (AbsT t1 t2) = AbsT (offset n t1) (offset n t2)

modifyBinds :: ([Binding] -> [Binding]) -> Ctx ()
modifyBinds f = modify $ \ctx -> ctx { bindings = f (bindings ctx) }

-- Error types and failure/rules monad

type Rules = StateT Ruleset (Either LambdaError)
type Ctx   = StateT Context (Either LambdaError)

runRules :: Rules a -> (a, Ruleset)
runRules x = case runStateT x M.empty of
  Left e  -> error (show e)
  Right r -> r

runCtx :: Ctx a -> a
runCtx x = case evalStateT x (Context [] 0) of
  Left e  -> error (show e)
  Right r -> r

data LambdaError
  = UnifyError Type Type
  | InfiniteType TVar Type
  | FreeError  VVar

instance Show LambdaError where
  show (FreeError v) =
    "Unbound free variable: " ++ v

  show (InfiniteType v t) =
    "Infinite type: t" ++ show v ++ " = " ++ show t

  show (UnifyError t1 t2) =
    "Unification failed: " ++ show t1 ++ " ~ " ++ show t2

left :: MonadTrans m => l -> m (Either l) a
left = lift . Left
