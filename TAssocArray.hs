{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module TAssociativeArray where
import Data.Singletons

singletons [d| data AssocArray a b = AssocArray [(a, b)] |]

type instance ('AssocArray xs) :==: ('AssocArray ys) = 
    If ((xs :==: '[ ]) :&&: (ys :==: '[ ]))
        True
        (If (xs :==: '[ ]) False (
            If (ys :==: '[ ])
                False
                (xs :==: (Reorder ys xs))
                ))

type instance '(a, b) :==: '(x, y) = (a :==: x) :&&: (b :==: y) 

type instance (Sing a) :==: (Sing b) = a :==: b

type family Fst (x :: (a,b)) :: a
type instance Fst '(a,b) = a

type family Snd (x :: (a,b)) :: b
type instance Snd '(a,b) = b

-- remove a DimSpec from a list, if it's in the list;
-- returns the new list and possibly the extracted element  
type family Extract (s :: (k, v)) (lst :: [(k, v)]) :: ([(k, v)], Maybe (k, v))
type instance Extract s '[] = '( '[], Nothing)
type instance Extract s (h ': t) =
  If (Fst s :==: Fst h)
   '(t, Just h)
   '(h ': Fst (Extract s t), Snd (Extract s t))

-- reorder 'a' in the order defined by 'b'. 
type family Reorder (a :: [(k, v)]) (b :: [(k, v')]) :: [(k, v)]
type instance Reorder x '[] = x
type instance Reorder x (h ': t) = Reorder' (Extract h x) t

type family Reorder' (scrut :: ([(k, v)], Maybe (k, v)))
                     (t :: [(k, v')])
                     :: [(k, v)]
type instance Reorder' '(lst, Nothing) t = Reorder lst t
type instance Reorder' '(lst, Just elt) t = elt ': (Reorder lst t)

{-
singletons [d| data TestValue = Test1
                              | Test2
                              | Test3
                         deriving(Eq, Show)
                         
               data TestKey = Key1
                             | Key2
                             | Key3
                             deriving(Eq, Show)
                         
                         |]

test :: ((a :==: b) ~ True) => Sing (a :: AssocArray k v) -> Sing (b :: AssocArray k v) -> Int
test = undefined

testA :: Sing ('AssocArray '[ '(Sing 'Key1, Sing 'Test3),
                      '(Sing 'Key1, Sing 'Test2), 
                      '(Sing 'Key2, Sing 'Test1)
                        ])
testA = undefined

testB :: Sing ('AssocArray '[ '(Sing 'Key2, Sing 'Test1),
                      '(Sing 'Key1, Sing 'Test2),
                      '(Sing 'Key1, Sing 'Test3)])
testB = undefined

testC :: Sing ('AssocArray '[ '(Sing 'Key2, Sing 'Test2),
                      '(Sing 'Key1, Sing 'Test2),
                      '(Sing 'Key1, Sing 'Test3)])
testC = undefined

test1 = test testA testB
-}

-- THIS won't compile
--test2 = test testA testC













