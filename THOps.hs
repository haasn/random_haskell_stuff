{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data Foo a b = a :^ b

foo :: Name
foo = '(:^)
