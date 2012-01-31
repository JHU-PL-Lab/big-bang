module Language.TinyBang.Test.Binders
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

idX = ident "x"
varX = A.Var idX

tests = TestLabel "Binder tests" $ TestList
  [ xPars "case 4 of {x:_ -> x = 2 in x}" $
          A.Case (A.PrimInt 4)
           [ A.Branch (Just idX) A.ChiAny $
             A.Assign (A.AIdent idX) (A.PrimInt 2) varX ]
  , xLexs "case 4 of {x:_ -> x = 2 in x}"
          [ TokCase
          , TokIntegerLiteral 4
          , TokOf
          , TokOpenBlock
          , TokIdentifier "x"
          , TokColon
          , TokUnder
          , TokArrow
          , TokIdentifier "x"
          , TokEquals
          , TokIntegerLiteral 2
          , TokIn
          , TokIdentifier "x"
          , TokCloseBlock
          ]

  , xEval "case () & 0 of {x:int -> x}"
          (A.VPrimInt 0)
  , xEval "case () & 0 of {x:unit -> x}"
          A.VPrimUnit
  , xEval "case () & 0 of {x:_ -> x}"
          (A.VOnion (A.VPrimInt 0) A.VPrimUnit)

  -- The next test should be a contradiction due to binder immutability.
  , xCont "case 4 of {x:_ -> x = 0 in x}"
  ]
