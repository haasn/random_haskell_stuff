triangle n = mapM_ (putStrLn . line) [1..max]
  where
    max = ceiling $ fromIntegral n/2
    line t = side ++ middle ++ side
      where
        side = replicate (max - t) ' '
        middle = replicate (t*2 - 1) '#'

