{-# LANGUAGE TypeOperators, FlexibleContexts, LambdaCase #-}

import Control.Applicative
import Control.Monad (unless, forever)
import Data.Machine hiding (addL, addR, tee)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.IO

readLines :: FilePath -> Machine IO T.Text
readLines f = construct $ request (openFile f ReadMode) >>= loop
  where loop h = do
        b <- request $ hIsEOF h
        if b then request (hClose h) else do
          request (T.hGetLine h) >>= yield
          loop h

writeLines :: Await T.Text m => FilePath -> Machine (m :+: IO) o
writeLines f = construct $ do
  h <- request . R $ openFile f WriteMode
  let loop = forever $ awaits L >>= request . R . T.hPutStrLn h
  loop <|> request (R $ hClose h) *> empty


addL :: Machine m a -> Machine ((->) a :+: n) b -> Machine (m :+: n) b
addL ma m = case m of
  Stop             -> Stop
  Yield o k        -> Yield o $ addL ma k
  Await f (L fh) ff -> case ma of
    Stop         -> addL empty ff
    Yield a k    -> addL k (f (fh a))
    Await g h fg -> Await (\a -> addL (g a) m) (L h) (addL fg m)

  Await f (R fh) ff -> Await (addL ma . f) (R fh) (addL ma ff)

addR :: Machine m a -> Machine (n :+: (->) a) b -> Machine (n :+: m) b
addR ma = fit flipT . addL ma . fit flipT
 where
  flipT :: (m :+: n) a -> (n :+: m) a
  flipT (L f) = R f
  flipT (R f) = L f

tee :: Machine m a -> Machine n b -> Tee a b c -> Machine (m :+: n) c
tee ma mb = addL ma . addR mb
