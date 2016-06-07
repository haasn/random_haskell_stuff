module Brainfuck.Compiler (Brainfuck(..), Program, compile) where

import Brainfuck.Parser
import Prelude hiding (Either(..))

import Control.Lens
import Control.Monad.Free

-- Higher level semantic graph

data Brainfuck n
  = Succ n | Pred n  -- Increment or decrement the current value
  | Next n | Prev n  -- Shift memory left or right
  | Read n | Write n -- Input or output the current value

  -- Branching semantic, used for both sides of loops
  | Branch { zero :: n, nonzero :: n }

type Program = Free Brainfuck ()

compile :: Code -> Program
compile = fst . bracket []

bracket :: [Program] -> Code -> (Program, [Program])
bracket [] []        = (Pure (), [])
bracket _  []        = error "Mismatched opening bracket"
bracket [] (Close:_) = error "Mismatched closing bracket"

-- Match a closing bracket: Pop a forward continuation, push backwards
bracket (c:cs) (Close : xs) = (Free (Branch n c), n:bs)
  where (n, bs) = bracket cs xs

-- Match an opening bracket: Pop a backwards continuation, push forwards
bracket cs (Open : xs) = (Free (Branch b n), bs)
  where (n, b:bs) = bracket (n:cs) xs

-- Match any other symbol in the trivial way
bracket cs (x:xs) = bracket cs xs & _1 %~ Free . f x
  where
    f Plus  = Succ; f Minus = Pred
    f Right = Next; f Left  = Prev
    f Comma = Read; f Dot   = Write
