import Control.Lens
import Control.Monad ((<=<), void)
import Language.Haskell.Interpreter

type Code = String -> IO String

code :: String
code = " let inc = iso words unwords._last._Show +~ 1 \
       \ in \\s -> inc s <$ print 0"

bootstrap :: MonadInterpreter m => String -> m a
bootstrap s = do
  c <- interpret s (as :: Code)
  liftIO (c s) >>= bootstrap

main :: IO ()
main = void . _Left print <=< runInterpreter $ do
  setImports ["Prelude", "Control.Applicative", "Control.Lens", "Text.Read"]
  bootstrap code
