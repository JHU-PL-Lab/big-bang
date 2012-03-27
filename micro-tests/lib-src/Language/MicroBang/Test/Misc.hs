module Language.MicroBang.Test.Misc
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.NameUtils
  ( lblTrue
  , lblFalse
  )
import Language.MicroBang.Test.ExpressionUtils
  ( multiAppl
  )
import qualified Language.MicroBang.Ast as A

tests :: (?debug :: Bool) => Test
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
                 (A.Label lblTrue A.PrimUnit)
                 ( A.VLabel lblTrue 0
                 , makeState [(0, A.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (A.Label lblFalse A.PrimUnit)
                 ( A.VLabel lblFalse 0
                 , makeState [(0, A.VPrimUnit)]
                 )
  ]