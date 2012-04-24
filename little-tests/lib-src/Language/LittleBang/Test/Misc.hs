module Language.LittleBang.Test.Misc
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
  ( llblTrue
  , llblFalse
  , tlblTrue
  , tlblFalse
  )
import Language.LittleBang.Test.ExpressionUtils
  ( multiAppl
  )
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ xPars "'s''t''r''i''n''g'" $
          multiAppl [ (LA.PrimChar 's')
                    , (LA.PrimChar 't')
                    , (LA.PrimChar 'r')
                    , (LA.PrimChar 'i')
                    , (LA.PrimChar 'n')
                    , (LA.PrimChar 'g')
                    ]
  , xNotC "x"
  , lexParseEval "`True ()"
                 [ TokLabelPrefix
                 , TokIdentifier "True"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (LA.Label llblTrue Nothing LA.PrimUnit)
                 ( TA.VLabel tlblTrue 0
                 , makeState [(0, TA.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (LA.Label llblFalse Nothing LA.PrimUnit)
                 ( TA.VLabel tlblFalse 0
                 , makeState [(0, TA.VPrimUnit)]
                 )
  ]
