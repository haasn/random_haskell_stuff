{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.Haskell.TH

assert :: Q Exp -> Q Exp
assert qexp = do
    pretty <- fmap show qexp
    [| case $qexp of False -> error pretty; _ -> id |]
