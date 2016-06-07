-- Typeclass

data Num' a = Num'
  { add :: a -> a -> a
  , mul :: a -> a -> a
  }

-- Instance

numInt :: Num' Int
numInt = Num' { add = (+), mul = (*) }

-- Polymorphic function

double :: Num' a -> a -> a
double dict x = add dict x x
