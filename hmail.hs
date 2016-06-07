import Control.Lens
import Data.List

import System.Environment
import System.Exit
import System.IO
import System.Process

main = do
  c  <- getContents
  as <- getArgs
  let ps = c^.to lines.folded.filtered ("From: " `isPrefixOf`).to email.to par
      par xs = ["-f", xs]

  (Just h,_,_,p) <- createProcess (proc "msmtp" $ as ++ ps) { std_in = CreatePipe }

  hPutStr h c >> hClose h
  waitForProcess p >>= exitWith

email :: String -> String
email l
  | "<" `isPrefixOf` r && ">" `isSuffixOf` r = init . tail $ r
  | otherwise = r
  where r = last (words l)
