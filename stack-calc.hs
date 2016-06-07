start :: (() -> r) -> r
start f = f ()

end :: (a, s) -> a
end = fst

push :: s -> a -> ((a, s) -> r) -> r
push s a f = f (a, s)

op2 :: (a -> b -> c) -> (a, (b, s)) -> ((c, s) -> r) -> r
op2 o (a, (b, s)) f = f (a `o` b, s)

add, mul :: Num a => (a, (a, s)) -> ((a, s) -> r) -> r
add = op2 (+)
mul = op2 (*)

example :: Integer
example = -- 35
  start
  push 2
  push 3
  add
  push 7
  mul
  end
