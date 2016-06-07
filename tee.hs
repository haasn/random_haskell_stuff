{-# LANGUAGE RankNTypes, TypeFamilies #-}

import Control.Applicative

import System.Environment (getArgs)
import System.IO (IOMode(..))

import Pipes
import Pipes.Safe
import Pipes.Safe.Prelude (withFile)
import Pipes.Prelude hiding (foldr)

append :: (MonadSafe m, Base m ~ IO) => FilePath -> Consumer' String m r
append f = withFile f AppendMode toHandle

main = do
 ps <- foldr (\f d -> tee (append f) >-> d) cat <$> getArgs
 runSafeT . runEffect $ stdinLn >-> ps >-> stdoutLn
