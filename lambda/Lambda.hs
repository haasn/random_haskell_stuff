{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Control.Applicative ((<$>), (<*>))

class (Show t, Eq t) => Typed c t | c -> t where
  getType :: c -> Type t

data Lambda c t
  = Unit
  | ConstL c
  | Free String
  | Abs String (Type t) (Lambda c t)
  | App (Lambda c t) (Lambda c t)
  | Pair (Lambda c t) (Lambda c t)
  | Fst (Lambda c t)
  | Snd (Lambda c t)
  deriving Show

data Type t
  = UnitT
  | AnyT
  | ConstT t
  | AbsT String (Type t) (Type t)
  | PairT (Type t) (Type t)
  deriving (Eq)

instance Show t => Show (Type t) where
  show UnitT        = "()"
  show AnyT         = "?"
  show (ConstT t)   = show t
  show (AbsT _ a b) = show a ++ " → " ++ show b
  show (PairT a b)  = "(" ++ show a ++ ", " ++ show b ++ ")"

lambdaType :: Typed c t => Lambda c t -> Either String (Type t)
lambdaType Unit = return UnitT
lambdaType (ConstL c) = return $ getType c
lambdaType (Abs n t1 m1) = AbsT n t1 <$> replace n t1 m1
lambdaType (Pair m1 m2) = PairT <$> lambdaType m1 <*> lambdaType m2
lambdaType (Free n) = return AnyT

lambdaType (App m1 m2) = do
  AbsT n t1 tr <- lambdaType m1
  t2           <- lambdaType m2
  if t1 == t2
    then return tr
    else fail $ "Type mismatch in function application: " ++
          show t1 ++ " ~ " ++ show t2

lambdaType (Fst m1) = do
  PairT t1 _ <- lambdaType m1
  return t1

lambdaType (Snd m1) = do
  PairT _ t2 <- lambdaType m1
  return t2

replace :: Typed c t => String -> Type t -> Lambda c t -> Either String (Type t)
replace n1 t1 (Free n2)
  | n1 == n2 = return t1
replace _ _ t2 = lambdaType t2

-- Example

data MyType = TBool | TABC
  deriving (Eq, Show)

data MyValues = Not | MyB Bool | A | B | C
  deriving (Eq, Show)

instance Typed MyValues MyType where
  getType Not = AbsT "b" (ConstT TBool) (ConstT TBool)

  getType A = ConstT TABC
  getType B = ConstT TABC
  getType C = ConstT TABC

  getType (MyB _) = ConstT TBool

myType :: Lambda MyValues MyType -> Type MyType
myType t = case lambdaType t of
  Right x -> x
  Left  e -> error e
