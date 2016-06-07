-- Natural numbers

data Nat = Zero | Succ Nat
  deriving Eq

instance Show Nat where
  show = show . toInteger

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero _    = LT
  compare _ Zero    = GT

  compare (Succ a) (Succ b) = compare a b

instance Real Nat where
  toRational n = toInteger n % 1

instance Enum Nat where
  succ = Succ

  pred (Succ n) = n
  pred Zero     = error "Nat.pred: Zero"

  toEnum   = fromInteger . toInteger
  fromEnum = fromInteger . toInteger

  enumFrom = iterate succ
  enumFromThen a b = iterate (+ step) a
    where step = b |- a

  enumFromTo a b
    | a >= b    = []
    | otherwise = a : enumFromTo (succ a) b

  enumFromThenTo a b c
    | a >= c    = []
    | otherwise = a : enumFromThenTo (a + step) (b + step) c
    where step = b |- a

instance Bounded Nat where
  minBound = Zero
  maxBound = infinity

instance Integral Nat where
  quotRem a b = (fromInteger a', fromInteger b')
    where (a', b') = quotRem (toInteger a) (toInteger b)
  toInteger Zero     = 0
  toInteger (Succ n) = 1 + toInteger n

instance Num Nat where
  Zero     + x = x
  (Succ s) + x = Succ (s + x)

  x        - Zero     = x
  (Succ s) - (Succ x) = s - x
  Zero     - (Succ _) = error "(Nat.-): Negative result"

  _        * Zero = Zero
  Zero     * _    = Zero
  (Succ n) * x    = x + (n * x)

  negate _ = error "Nat.negate"
  abs      = id

  signum Zero = 0
  signum _    = 1

  fromInteger n = case compare n 0 of
    LT -> error "Nat.fromInteger: Negative number"
    EQ -> Zero
    GT -> Succ (fromInteger (n - 1))

-- Bounded subtraction
(|-) :: Nat -> Nat -> Nat
Zero     |- _        = Zero
x        |- Zero     = x
(Succ a) |- (Succ b) = a |- b

-- Infinity
infinity :: Nat
infinity = fix Succ
