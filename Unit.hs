{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, GADTs, PolyKinds     #-}

import Prelude hiding (Int, (+), (-), (*), (/), (^), recip)
import qualified Prelude as P

-- Examples

dist :: Num a => a :@ Meter
dist = u 5*meter

time :: Double :@ Second
time = u 3.14*second

speed :: Double :@ (Meter/Second)
speed = dist / time

area :: Integer :@ (Meter^I2)
area = dist ^ S (S Z)

{-
invalid = m^e2 + s^e3

> Couldn't match type 'NS ('NS ('NS 'N0)) with 'N0
> In the expression: m ^ e2 + s ^ e3
> In an equation for `invalid': invalid = m ^ e2 + s ^ e3
-}

celsius :: Fractional a => a:@"Celsius" -> a:@Kelvin
celsius (U a) = U (a P.+ 273.15)

-- Type-level natural numbers and addition

data Nat = N0 | NS Nat

type family AddN (n :: Nat) (m :: Nat) :: Nat
type instance AddN  N0    m = m
type instance AddN (NS n) m = NS (AddN n m)

type family MulN (n :: Nat) (m :: Nat) :: Nat
type instance MulN  N0    m = N0
type instance MulN (NS n) m = AddN m (MulN n m)

type N1 = NS N0
type N2 = NS N1
type N3 = NS N2

-- Type-level integers (implemented as pairs of nats n-m)

data Int = Nat :- Nat

type family AddI (n :: Int) (m :: Int) :: Int
type instance AddI (a:-b) (x:-y) = AddN a x :- AddN b y

-- (a-b)(x-y) = a(x-y)-b(x-y) = ax-ay-bx+by = (ax+by)-(ay+bx)
type family MulI (n :: Int) (m :: Int) :: Int
type instance MulI (a:-b) (x:-y) =
  AddN (MulN a x) (MulN b y) :- AddN (MulN a y) (MulN b x)

type family NegI (n :: Int) :: Int
type instance NegI (n:-m) = (m:-n)

class EqI (n :: Int) (m :: Int)
instance (AddN a y ~ AddN b x) => EqI (a:-b) (x:-y)

type I0 = N0 :- N0
type I1 = N1 :- N0
type I2 = N2 :- N0
type I3 = N3 :- N0

-- Value-level nats that carry along type nat for exponentiation

data Exp :: Int -> * where
  Z :: Exp I0
  S :: Exp n -> Exp (AddI I1 n)

e0 :: Exp I0
e0 = Z

e1 :: Exp I1
e1 = S e0

e2 :: Exp I2
e2 = S e1

e3 :: Exp I3
e3 = S e2

-- Type-level SI record for storing unit polynomials

data Unit = SI Int Int Int -- Seconds, Meters, Kelvin

type family Recip (n :: Unit) :: Unit
type instance Recip (SI a b c) = SI (NegI a) (NegI b) (NegI c)

type family (n :: Unit) * (m :: Unit) :: Unit
type instance SI a b c * SI x y z = SI (AddI a x) (AddI b y) (AddI c z)

type family (n :: Unit) / (m :: Unit) :: Unit
type instance n / m = n * Recip m

type family (n :: Unit) ^ (m :: Int)  :: Unit
type instance SI a b c ^ e = SI (MulI e a) (MulI e b) (MulI e c)

class (n :: Unit) == (m :: Unit)
instance (EqI a x, EqI b y, EqI c z) => SI a b c == SI x y z

-- Data type for enriching numbers with units

data a :@ (u :: u) = U a
  deriving (Eq, Ord)

instance Show a => Show (a :@ u) where
  show (U a) = show a

type Second = SI I1 I0 I0
second :: Num a => a :@ Second
second = U 1

type Meter = SI I0 I1 I0
meter :: Num a => a :@ Meter
meter = U 1

type Kelvin = SI I0 I0 I1
kelvin :: Num a => a :@ Kelvin
kelvin = U 1

type One = SI I0 I0 I0
u :: a -> a :@ One
u = U

unitCoerce :: a:@u -> a:@v
unitCoerce (U a) = U a

-- Type-enriched calculations

(+) :: (Num a, u == v) => a:@u -> a:@v -> a:@u
U a + U b = U (a P.+ b)
infixl 6 +

(-) :: (Num a, u == v) => a:@u -> a:@v -> a:@u
U a - U b = U (a P.- b)
infixl 6 -

(*) :: Num a => a:@u -> a:@v -> a:@(u*v)
U a * U b = U (a P.* b)
infixl 7 *

(/) :: Fractional a => a:@u -> a:@v -> a:@(u/v)
U a / U b = U (a P./ b)
infixl 7 /

(^) :: Num a => a:@u -> Exp e -> a:@(u^e)
U a ^ e = U (a P.^ toNum e)
 where
  toNum :: Num a => Exp e -> a
  toNum  Z    = 0
  toNum (S n) = 1 P.+ toNum n
infixr 8 ^

recip :: Fractional a => a:@u -> a:@(Recip u)
recip (U a) = U (P.recip a)
