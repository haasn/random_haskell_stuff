{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

import qualified Data.Map as M
import Data.Monoid

import Control.Lens
import Control.Monad.Continue
import Control.Monad.State

newtype Label = Label Int
  deriving (Num, Eq, Ord, Show)

instance Monoid Label where
  mempty = Label 0
  mappend (Label 0) = id
  mappend x         = const x

newtype VN a = VN (StateT Label (ContinueT Label (M.Map Label) IO) a)
  deriving (Monad, MonadContinue Label (M.Map Label), MonadIO)

fresh :: VN Label
fresh = VN $ id <+= 1

label :: VN Label
label = do
  l <- fresh
  continue_ (M.singleton l ())
  return l

goto :: Label -> VN ()
goto = suspend

test :: VN ()
test = let o = liftIO . putStrLn in do
  o "before label"
  l <- label
  o "after label"
  s <- liftIO getLine
  when (s == "loop") (goto l)
  o $ "Wow, you entered: " ++ s
  o "that is exciting! <END>"

runVN :: VN () -> IO ()
runVN (VN s) = go (evalStateT s mempty)
 where
  go c = do
    (e, m) <- runContinueT c
    print e
    case e of
      Right x -> return x
      Left  l -> case m ^? ix l of
        Nothing -> error "should never happen"
        Just c' -> go (addCont_ (M.singleton l c') >> c')
