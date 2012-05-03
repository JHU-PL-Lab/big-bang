module Language.LittleBang.Test.Misc
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
  ( lblTrue
  , lblFalse
  )
import Language.LittleBang.Test.ExpressionUtils
  ( multiAppl
  )
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ xPars "'s''t''r''i''n''g'" $
          multiAppl $ (map astwrap $
                    [ (TA.PrimChar 's')
                    , (TA.PrimChar 't')
                    , (TA.PrimChar 'r')
                    , (TA.PrimChar 'i')
                    , (TA.PrimChar 'n')
                    , (TA.PrimChar 'g')
                    ] :: [LA.Expr])
  , xNotC "x"
  , lexParseEval "`True ()"
                 [ TokLabelPrefix
                 , TokIdentifier "True"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (astwrap $ TA.Label lblTrue Nothing $ astwrap TA.PrimUnit
                    :: LA.Expr)
                 ( TA.VLabel lblTrue 0 :: TA.Value IA.Expr
                 , makeState [(0, TA.VPrimUnit)]
                 )
  , lexParseEval "`False ()"
                 [ TokLabelPrefix
                 , TokIdentifier "False"
                 , TokOpenParen
                 , TokCloseParen
                 ]
                 (astwrap $ TA.Label lblFalse Nothing $ astwrap TA.PrimUnit
                    :: LA.Expr)
                 ( TA.VLabel lblFalse 0 :: TA.Value IA.Expr
                 , makeState [(0, TA.VPrimUnit)]
                 )
  ]
