{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies, PolyKinds
           , OverlappingInstances, FunctionalDependencies, TypeOperators
           , FlexibleInstances, UndecidableInstances, TemplateHaskell
           , RankNTypes #-}

import Control.Lens
import GHC.TypeLits

-- Short version of Proxy for labels
data L a = L

-- Reimplementation of overloaded records machinery
class Has (s :: Symbol) t a | s t -> a where
  this :: label s -> Lens' t a

class t `Extends` b | t -> b where
  base :: Lens' t b

class t `Implements` i where
  interface :: t -> i

-- Fallback base instance
instance (t `Extends` b, Has s b a) => Has s t a where
  this l = base.this l

-- Some example ADTs
data Foo b = Foo
  { _fooX :: Int
  , _fooY :: Char
  , _fooBase :: b
  }
makeLenses ''Foo

instance Has "x" (Foo b) Int  where this _ = fooX
instance Has "y" (Foo b) Char where this _ = fooY
instance Foo b `Extends` b where base = fooBase

data Bar b = Bar
  { _barX :: String
  , _barZ :: Bool
  , _barBase :: b
  }
makeLenses ''Bar

instance Has "x" (Bar b) String where this _ = barX
instance Has "z" (Bar b) Bool   where this _ = barZ
instance Bar b `Extends` b where base = barBase

-- Example values

foo :: Foo ()
foo = Foo 3 'x' ()

bar :: Bar ()
bar = Bar "hello" True ()

foobar :: Foo (Bar ())
foobar = foo { _fooBase = bar }

barfoo :: Bar (Foo ())
barfoo = bar { _barBase = foo }

-- Example interfaces

data IX a = IX { _ixX :: a }
makeLenses ''IX

instance Has "x" (IX a) a where this _ = ixX

instance Foo b `Implements` IX Int where
  interface (Foo x _ _) = IX x

instance Bar b `Implements` IX String where
  interface (Bar x _ _) = IX x
