{-# LANGUAGE LambdaCase #-}

import Control.Lens

import System.Directory (copyFile, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Lens (filename)

main :: IO ()
main = getArgs >>= \case
  f:t:_ -> multiCopy f t
  _     -> putStrLn "Usage: multicopy <file> <target>"

multiCopy :: FilePath -> FilePath -> IO ()
multiCopy file target = do
  dir <- getCurrentDirectory
  let copy s = copyFile (dir </> s) (target </> s^.filename)

  file ^! act readFile.to lines.traverse.act copy
