import Data.List (permutations)

{-
cwords :: [String]
cwords = words $ "The 19 words in the list below are all distinct and share a common theme can you identify the words and the theme and place 15 of them into the grid" ++
  "after the 15 are all placed onto the grid there will be four words remaining 2 5 letter words and 2 6 letter words"
-}

cwords :: [String]
cwords = words $
  -- five letter palindromes
  "alula anana civic deked deled dered dewed kaiak kayak lemel level madam malam minim radar refer rotor sagas samas sedes seles semes seres sexes shahs simis siris solos stats stets stots sulus susus tenet torot"
  ++

  -- six letter palindromes
  "denned hallah mallam marram pippip pullup redder renner revver selles sesses succus terret tirrit tuttut"

puzzle :: [String]-> Bool
puzzle (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:_) = and
  [ a !! 3 == b !! 2
  , a !! 5 == d !! 1
  , c !! 0 == d !! 0
  , c !! 2 == e !! 3
  , f !! 0 == d !! 4
  , f !! 2 == g !! 1
  , h !! 2 == i !! 0
  , g !! 4 == h !! 3
  , j !! 2 == i !! 3
  , k !! 1 == j !! 4
  , k !! 4 == l !! 0
  , l !! 2 == m !! 3
  , n !! 2 == o !! 1
  , n !! 4 == k !! 5
  ]

tryPuzzle :: [String] -> [[String]]
tryPuzzle list = do
  (e:c:f:g:i:j:n:o:_) <- fives
  (a:b:d:h:k:l:m:_  ) <- sixes

  let combo = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]

  case puzzle combo of
    True  -> return combo
    False -> []


  where five = filter ((==5) . length) list
        six  = filter ((==6) . length) list

        -- Five length: E, C, F, G, I, J, N, O
        -- Six  length: A, B, D, H, K, L, M

        fives = permutations five
        sixes = permutations six

main = print $ tryPuzzle cwords
