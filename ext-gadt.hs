{-# LANGUAGE GADTs #-}

-- data
data Lit a = Lit a

data Add x y r where
    Add :: x Int -> y Int -> Add x y Int

data Print s r where
    Print :: s String -> Print s ()

data Ap f x r where
    Ap :: f (a -> b) -> x a -> Ap f x b

-- operations
class Eval x where
    eval :: x a -> IO a

instance Eval Lit where
    eval (Lit a) = return a

instance (Eval x, Eval y) => Eval (Add x y) where
    eval (Add x y) = do x' <- eval x; y' <- eval y; return (x' + y')

instance Eval s => Eval (Print s) where
    eval (Print s) = eval s >>= putStrLn

instance (Eval f, Eval x) => Eval (Ap f x) where
    eval (Ap f x) = do f' <- eval f; x' <- eval x; return (f' x')


class Size x where
    size :: x t -> Int

instance Size Lit where
    size (Lit _) = 1

instance (Size x, Size y) => Size (Add x y) where
    size (Add x y) = 1 + size x + size y

instance Size s => Size (Print s) where
    size (Print s) = 1 + size s

instance (Size f, Size x) => Size (Ap f x) where
    size (Ap f x) = 1 + size f + size x


-- example

--foo :: Print (Ap Lit (Add Lit Lit)) ()
foo = Print $ Ap (Lit show) (Add (Lit 3) (Lit 5))

main = do print (size foo); eval foo
