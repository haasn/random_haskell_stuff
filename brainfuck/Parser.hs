module Brainfuck.Parser where

import Prelude hiding (Either(..))

-- Low level syntax form

data Instr = Plus | Minus | Right | Left | Comma | Dot | Open | Close
type Code = [Instr]

parse :: String -> Code
parse = concatMap (maybe [] return . (`lookup` symbols))
  where symbols = [ ('+', Plus ), ('-', Minus), ('<', Left), ('>', Right)
                  , (',', Comma), ('.', Dot  ), ('[', Open), (']', Close) ]
