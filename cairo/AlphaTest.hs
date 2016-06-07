import Graphics.Rendering.Cairo

main = withImageSurface FormatARGB32 400 400 $ \s -> do
  renderWith s (setSourceRGBA 1 0 0 0.5 >> paint)
  surfaceWriteToPNG s "/tmp/output.png"
