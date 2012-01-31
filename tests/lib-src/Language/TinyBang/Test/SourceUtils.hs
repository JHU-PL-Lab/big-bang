module Language.TinyBang.Test.SourceUtils
( srcY
, srcMultiAppl
)
where

import Language.TinyBang.Test.UtilFunctions (TinyBangCode)

srcY  :: TinyBangCode
srcY  = "fun body ->"
        ++ " (fun f -> fun arg -> f f arg)"
        ++ " (fun this -> fun arg -> body (this this) arg)"

srcMultiAppl :: [TinyBangCode] -> TinyBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

