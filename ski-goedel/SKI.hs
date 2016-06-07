import Text.Parsec
import Text.Parsec.String

-- | The SK combinatorial calculus
data SK = S | K | Free Char | App SK SK deriving Eq

instance Show SK where
  show S         = "S"
  show K         = "K"
  show (Free c)  = [c]
  show (App a b) = '`' : show a ++ show b

-- | The Lambda calculus is here to be considered a superset of the SK
--   calculus because it reduces the need to find free variables
data Lambda = Sλ | Kλ | Freeλ Char | Absλ Char Lambda | Appλ Lambda Lambda

instance Show Lambda where
  show (Sλ)       = show S
  show (Kλ)       = show K
  show (Freeλ c)  = [c]
  show (Absλ c l) = concat ["λ", [c], " → ", show l]
  show (Appλ a b) = concat ["(", show a, " ", show b, ")"]

-- A parser for lambda terms
parseλ :: Parser Lambda
parseλ = parseS <|> parseK <|> parseFree <|> parseAbs <|> parseApp
  where
    parseS    = char 'S' >> return Sλ
    parseK    = char 'K' >> return Kλ
    parseFree = Freeλ `fmap` parseVar
    parseAbs  = do
      char 'λ' <|> char '\\'
      name <- parseVar
      char '→' <|> char '.'
      exp  <- parseλ
      return (Absλ name exp)

    parseApp = do
      char '('
      e1 <- parseλ
      e2 <- parseλ
      char ')'
      return $ Appλ e1 e2

    parseVar  = foldl1 (<|>) (map char ['a'..'z'])

readλ :: String -> Either ParseError Lambda
readλ = parse parseTerm "(lambda)"
  where
    parseTerm = do
      l <- parseλ
      eof
      return l

-- | Convert a lambda term to its unlambda form
unlambda :: String -> String
unlambda s = case readλ s of
  Left e  -> show e
  Right l -> show $ fixReduce (λtoSK l)

-- | Like unlambda but doesn't attempt to reduce first
unlambda' :: String -> String
unlambda' s = case readλ s of
  Left e  -> show e
  Right l -> show (λtoSK l)

-- | Determine if a variable is free in an expression
free :: Char -> Lambda -> Bool
free c Sλ = False
free c Kλ = False
free c (Freeλ c') = c == c'
free c (Appλ a b) = free c a || free c b
free c (Absλ c' l)
  | c == c'   = False
  | otherwise = free c l

-- | Injection into lambda calculus
toλ :: SK -> Lambda
toλ (Free c)  = Freeλ c
toλ (App a b) = Appλ (toλ a) (toλ b)
toλ S = Sλ
toλ K = Kλ

-- | Abstraction elimination and transformation to SK term
λtoSK :: Lambda -> SK

-- Rule 0: Equivalences
λtoSK Sλ = S
λtoSK Kλ = K

-- Rule 1: Free variables
λtoSK (Freeλ c)
  = Free c

-- Rule 2: Applications
λtoSK (Appλ e1 e2)
  = App (λtoSK e1) (λtoSK e2)

-- Rule 3: Constant abstractions
λtoSK (Absλ c e)
  | not (free c e) = App K (λtoSK e)

-- Rule 4: Identity abstraction
λtoSK (Absλ c (Freeλ c'))
  | c == c' = App (App S K) K

-- Rule 5: Dyadic abstraction
λtoSK (Absλ x y@(Absλ _ e))
  | free x e = λtoSK (Absλ x (λtoλ y))

-- η-reduction
λtoSK (Absλ c (Appλ e (Freeλ c')))
  | c == c' && not (free c e) = λtoSK e

-- Rule 6: Application abstraction
λtoSK (Absλ c (Appλ e1 e2))
  = App (App S (λtoSK (Absλ c e1))) (λtoSK (Absλ c e2))

-- | Eliminate abstractions in a lambda term
λtoλ :: Lambda -> Lambda
λtoλ = toλ . λtoSK


-- | Reduce an SK term
reduce :: SK -> SK

-- Identity application
reduce (App (App (App S K) K) y) = y

-- Fully applied forms
reduce (App (App K x) y)
  = reduce x

reduce (App (App (App S x) y) z)
  = reduce (App (App (reduce x) (reduce z)) (App (reduce y) (reduce z)))

-- Unknown applied forms
reduce (App x y) = App (reduce x) (reduce y)

-- All other forms are to be considered fully reduced
reduce x = x


-- | Find the fixed point of reduce starting with a given term
--   Note that this won't terminate for cyclic reductions
fixReduce :: SK -> SK
fixReduce e
  | e == e' = e
  | otherwise = fixReduce e'
  where e' = reduce e
