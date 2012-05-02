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
import Utils.Language.Ast

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ xPars "'s''t''r''i''n''g'" $
          multiAppl $ (map astwrap $
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
                 (astwrap $ A.Label lblTrue Nothing $ astwrap A.PrimUnit
                    :: A.Expr)
                 ( A.VLabel lblTrue 0 :: A.Value A.Expr
                 , makeState [(0, A.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (astwrap $ A.Label lblFalse Nothing $ astwrap A.PrimUnit)
                 ( A.VLabel lblFalse 0 :: A.Value A.Expr
                 , makeState [(0, A.VPrimUnit)]
                 )
  ]