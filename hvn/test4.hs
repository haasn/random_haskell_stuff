import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Wire
import Control.Wire.Session

import Data.Word

import qualified Graphics.UI.SDL as SDL

sdlSession :: MonadIO m => Session m (Timed Word32 ())
sdlSession = Session $ do
  t <- liftIO SDL.getTicks
  return (Timed t (), sdlSession)

main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  testGame screen

testGame :: SDL.Surface -> IO ()
testGame s = do
  (
