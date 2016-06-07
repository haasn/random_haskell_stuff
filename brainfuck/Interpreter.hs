{-# LANGUAGE TypeOperators #-}
module Brainfuck.Interpreter (Input, Output, Cell, interpret) where

import Brainfuck.Compiler

import Control.Lens
import Control.Monad.Free
import Control.Monad.RWS

import Data.Maybe (fromMaybe)
import qualified Data.Stream.Infinite as S
import Data.Word (Word8)

-- RWS-based interpreter

type Cell   = Word8
type Input  = S.Stream Cell
type Output = [Cell]
type Memory = Top :> [Cell] :> Cell -- list zipper

type Interpreter = RWS Input Output Memory ()

-- Initial memory configuration
initial :: Memory
initial = zipper (replicate 30000 0) % fromWithin traverse

interpret :: Input -> Program -> Output
interpret i p = snd $ execRWS (run p) i initial

-- Evaluation function
run :: Program -> Interpreter
run (Pure _) = return ()
run (Free f) = case f of
  Succ n -> focus += 1       >> run n
  Pred n -> focus -= 1       >> run n
  Next n -> modify wrapRight >> run n
  Prev n -> modify wrapLeft  >> run n

  Read n -> do
    focus <~ asks S.head
    local S.tail $ run n

  Write n -> do
    tell . return =<< use focus
    run n

  Branch z n -> do
    c <- use focus
    run $ case c of 0 -> z; _ -> n

-- Zipper helpers

wrapRight, wrapLeft :: (a :> b) -> (a :> b)
wrapRight = liftM2 fromMaybe leftmost  right
wrapLeft  = liftM2 fromMaybe rightmost left
