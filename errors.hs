{-# LANGUAGE DeriveFunctor, LambdaCase #-}

import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Traversable (traverse)

import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Errors e a = Errors { runErrors :: Either e a }
  deriving (Show, Functor)

instance Semigroup e => Applicative (Errors e) where
  pure = Errors . Right

  Errors (Left x) <*> Errors (Left y) = Errors $ Left (x <> y)
  Errors (Left x) <*> Errors (Right _) = Errors (Left x)
  Errors (Right f) <*> x = fmap f x

error' :: String -> Errors [String] a
error' = Errors . Left . pure

-- Short example

constant :: String -> Maybe Double
constant = \case
  "pi" -> pure pi
  "answer" -> pure 42
  s -> Nothing

num :: String -> Maybe Double
num = readMaybe

parse :: String -> Either [String] [Double]
parse = runErrors . traverse f . words
  where f x = maybe (error' x) pure $ constant x <|> num x

main :: IO ()
main = do
  input <- unwords <$> getArgs
  case parse input of
    Right nums -> putStrLn "Numbers:" >> mapM_ print nums
    Left errors -> mapM_ (putStrLn . ("Unable to parse: "++)) errors
