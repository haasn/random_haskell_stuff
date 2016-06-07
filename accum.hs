import Control.Applicative
import Control.Proxy

main :: IO ()
main = runProxy $ stdinS >-> fold >-> stdoutD

-- How come this doesn't exist?
fold :: (Monad m, Monoid a, Proxy p) => () -> Pipe p a a m r
fold () = runIdentityP $ go mempty
  where go x = mappend x <$> request () >>= liftA2 (>>) respond go
