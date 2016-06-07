{-# LANGUAGE TemplateHaskell #-}

import Harpy.CodeGenMonad
import Harpy.X86Assembler
import Foreign
import Control.Monad.Trans
import System.IO.Unsafe

mulAsm :: CodeGen e s ()
mulAsm = do ensureBufferSize 32
            mov eax edi
            imul InPlace eax esi
            ret

callDecl "callMulAsm" [t|Int32 -> Int32 -> IO Int32|]

multiply :: Int32 -> Int32 -> Int32
multiply x y = unsafePerformIO $ do
    (_, Right res) <- runCodeGen (mulAsm >> callMulAsm x y) () ()
    return res
