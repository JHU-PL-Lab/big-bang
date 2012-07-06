module Language.TinyBang.Test.Misc
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
  ( lblTrue
  , lblFalse
  )
import Language.TinyBang.Test.ExpressionUtils
  ( multiAppl
  )
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Config as Cfg
import Data.ExtensibleVariant

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ xPars "'s''t''r''i''n''g'" $
          multiAppl $ (map inj $
                    [ (A.PrimChar 's')
                    , (A.PrimChar 't')
                    , (A.PrimChar 'r')
                    , (A.PrimChar 'i')
                    , (A.PrimChar 'n')
                    , (A.PrimChar 'g')
                    ] :: [A.Expr])
  , xNotC "x"
  , lexParseEval "`True ()"
                 [ TokLabelPrefix
                 , TokIdentifier "True"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (inj $ A.Label lblTrue Nothing $ inj A.PrimUnit
                    :: A.Expr)
                 ( A.VLabel lblTrue 0 :: A.Value IA.Expr
                 , makeState [(0, A.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (inj $ A.Label lblFalse Nothing $ inj A.PrimUnit)
                 ( A.VLabel lblFalse 0 :: A.Value IA.Expr
                 , makeState [(0, A.VPrimUnit)]
                 )
  ]