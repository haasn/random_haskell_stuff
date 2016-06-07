import Data.Ratio

scale :: (RealFrac a, Floating a, Num b) => a -> b
scale x = 10 ^ ceiling (logBase 10 x)

f :: Rational -> Rational
f x = f * scale (fromIntegral df)
    where (b, f) = properFraction x
          (nf,df) = (numerator f, denominator f)
