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
import qualified Language.TinyBang.Config as Cfg

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ xPars "'s''t''r''i''n''g'" $
          multiAppl [ (A.PrimChar 's')
                    , (A.PrimChar 't')
                    , (A.PrimChar 'r')
                    , (A.PrimChar 'i')
                    , (A.PrimChar 'n')
                    , (A.PrimChar 'g')
                    ]
  , xNotC "x"
  , lexParseEval "`True ()"
                 [ TokLabelPrefix
                 , TokIdentifier "True"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (A.Label lblTrue Nothing A.PrimUnit)
                 ( A.VLabel lblTrue 0
                 , makeState [(0, A.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (A.Label lblFalse Nothing A.PrimUnit)
                 ( A.VLabel lblFalse 0
                 , makeState [(0, A.VPrimUnit)]
                 )
  ]
