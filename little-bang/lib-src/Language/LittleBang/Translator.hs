module Language.LittleBang.Translator
( translateLittleBangToTinyBangNested
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBangNested.Ast as TBN

-- TODO: implement.  Note that this is probably wrong; TBN.Expr should be in
--       some monad.
translateLittleBangToTinyBangNested :: LB.Expr -> TBN.Expr
translateLittleBangToTinyBangNested = undefined
