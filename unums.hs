import Data.Bits
import Data.Char (intToDigit)
import Data.Fixed
import Numeric (showIntAtBase)

class Universal a where
    isInfinity  :: a -> Bool
    isZero      :: a -> Bool
    isNegative  :: a -> Bool
    isUncertain :: a -> Bool

newtype Unum n = Unum n

instance FiniteBits n => Universal (Unum n) where
    isInfinity  (Unum n) = n == bit (highBit n)
    isZero      (Unum n) = n == zeroBits
    isNegative  (Unum n) = testBit n (highBit n)
    isUncertain (Unum n) = testBit n 0

highBit :: FiniteBits b => b -> Int
highBit b = finiteBitSize b - 1

{-
getLattice :: (HasResolution e, FiniteBits n) => Unum e n -> [n]
getLattice u = [
-}

negateUnum :: Num n => Unum n -> Unum n
negateUnum (Unum n) = Unum (negate n)

recipUnum :: (Num n, FiniteBits n) => Unum n -> Unum n
recipUnum (Unum n) = Unum $ complementBit (complement n) (highBit n) + 1

showUnum :: (FiniteBits n, Integral n, Show n) => Unum n -> String
showUnum u@(Unum n)
    | isInfinity  u = "Infinity"
    | isZero      u = "0"
    | isUncertain u = "(" ++ showUnum (Unum (n-1)) ++ "," ++ showUnum (Unum (n+1)) ++ ")"
    | isNegative  u = '-' : showUnum (negateUnum u)
    | otherwise     = show $ 2 ^^ (n `shiftR` 1 - bit (highBit n - 2))
