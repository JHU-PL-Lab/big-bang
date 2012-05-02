module Language.LittleBang.Test.Functions
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
  ( idX, idSelf
  )
import Language.LittleBang.Test.ExpressionUtils
  ( varX, varSelf
  )
import Language.LittleBang.Test.ValueUtils
  ( identFuncX
  )
import Language.LittleBang.Test.SourceUtils
  ( srcY
  , srcMultiAppl
  )

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

selfIdentFuncX = TA.VFunc idSelf $ astwrap $ TA.Func idX varX

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test functions" $ TestList
  [ xvEval "fun x -> x"
          selfIdentFuncX
  , xvEval "(fun x -> x) (fun x -> x)"
          selfIdentFuncX
  , xvEval "(fun y -> y) (fun x -> x)"
          selfIdentFuncX
  , xvEval "def x = fun x -> x in x x"
          selfIdentFuncX
  , xType srcY
  , xvEval "fun x -> x x"
          (TA.VFunc idSelf $ astwrap $ TA.Func idX $ astwrap $
            TA.Appl (astwrap $ TA.Appl varX $ astwrap TA.EmptyOnion) varX)
  , xType "(fun x -> x x) (fun x -> x x)"
  , xType "def omega = fun x -> x x in omega omega"

  -- Ensure that functions are correctly polymorphic when stored in cells
  , xType "def f = (fun x -> x) in (f 1) & (f ())"

  -- Ensure that constraints propogate into functions properly
  , xCont "(fun f -> f ()) (fun x -> 1 + x)"
  , xCont "(fun x -> x + 1) 'a'"

  -- Test that application requires that the first argument be a function
  , xCont "1 'x'"

  -- Ensure that constraints from functions propagate correctly from cells
  , xCont "def f = (fun x -> case x of { int -> x }) in (f 1) & (f ())"

  -- Test typechecking of some pathological functions
  , xType $ srcMultiAppl
      [srcY, "fun this -> fun x -> this (`A x & `B x)"]
  , xType $ srcMultiAppl
      [srcY, "fun this -> fun x -> this (`A x & `B x)", "0"]
  , xType $ srcMultiAppl
      [srcY, "fun this -> fun x -> this (`A x & `B x)", "()"]
  , xType $ srcMultiAppl
      [srcY, "fun this -> fun x -> this (`A x & `B x)", "`A () & `B ()"]
  , xType $ srcMultiAppl
      [srcY, "fun this -> fun x -> this (`A x & `B x)", srcY]

  -- Check that variable closed-ness works in functions
  , xNotC "(fun x -> n + 2)"
  , xPars "fun x -> case x of {`True a -> 1; `False a -> 0}"
          (astwrap $ LA.Func idX $ astwrap $
              LA.Case varX
                     [ TA.Branch
                        (TA.ChiTopBind $ TA.ChiUnbound $
                              (TA.ChiLabelShallow (labelName "True")
                                          (ident "a")))
                              (astwrap $ TA.PrimInt 1)
                     , TA.Branch
                        (TA.ChiTopBind $ TA.ChiUnbound $
                              (TA.ChiLabelShallow (labelName "False")
                                          (ident "a")))
                              (astwrap $ TA.PrimInt 0)
                     ])
  ]
