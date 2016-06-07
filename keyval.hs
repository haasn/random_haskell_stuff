import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Gdk.Keys

foreign import ccall safe "gdk_unicode_to_keyval"
  gdk_unicode_to_keyval :: (CUInt -> (IO CUInt))

keyvalFromChar :: Char -> IO (Maybe KeyVal)
keyvalFromChar char = do
  code <- gdk_unicode_to_keyval $ fromIntegral $ fromEnum char
  if code == 0 then return Nothing
               else return $ Just $ fromIntegral code

keys :: [Char]
keys = "¹₁²₂³₃¤£€₅¼₆½₇¾₈‘₉’±¥≈×÷äÄåÅéÉ®þÞüÜúÚíÍóÓöÖ«≪»≫áÁß§ðÐēHūōøØ“°”¨¡¬¦æÆā©¢ñÑµçÇ≠≤¿≥"

lookupChar :: Char -> IO ()
lookupChar c = do
  putStr (c : ": ")
  code <- keyvalFromChar c
  case code of
    Nothing -> putStrLn "not found"
    Just c  -> keyvalName c >>= putStrLn

main = mapM lookupChar keys
