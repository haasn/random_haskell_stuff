import System.IO
import Directory
import System.Environment

main = do
    args <- listToMaybe <$> getArgs
    pDirContents =<< maybe getCurrentDirectory return args

pDirConts d = getDirectoryContents d >>= mapM_ putStrLn
