import Brainfuck.Parser
import Brainfuck.Compiler
import Brainfuck.Interpreter

import Control.Applicative
import Control.Monad (join)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Stream.Infinite as S

import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  as <- getArgs
  case as of
    -- STDIN is program
    [ ] -> do
      hSetBuffering stdin NoBuffering
      getContents >>= run noInput

    -- STDIN is input
    [f] -> join $ run <$> getInput <*> readFile f

    -- Malformed command line
    _ -> putStrLn "Usage: brainfuck [program]"

run :: Input -> String -> IO ()
run i = mapM_ putByte . interpret i . compile . parse
  where putByte = BS.putStr . BS.pack . return

-- EOF is represented as 0
getInput :: IO Input
getInput = f <$> BS.getContents
  where f s = S.fromList (BS.unpack s ++ repeat 0)

noInput :: Input
noInput = S.repeat 0
