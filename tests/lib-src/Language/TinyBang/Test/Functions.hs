module Language.TinyBang.Test.Functions
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
  ( idX
  )
import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import Language.TinyBang.Test.ValueUtils
  ( identFuncX
  )
import Language.TinyBang.Test.SourceUtils
  ( srcY
  , srcMultiAppl
  )

import qualified Language.TinyBang.Ast as A

tests :: (?debug :: Bool) => Test
tests = TestLabel "Test functions" $ TestList
  [ xEval "fun x -> x"
          identFuncX
  , xEval "(fun x -> x) (fun x -> x)"
          identFuncX
  , xEval "(fun y -> y) (fun x -> x)"
          identFuncX
  , xEval "def x = fun x -> x in x x"
          identFuncX
  , xType srcY
  , xEval "fun x -> x x"
          (A.VFunc idX $ A.Appl varX varX)
  , xType "(fun x -> x x) (fun x -> x x)"
  , xType "def omega = fun x -> x x in omega omega"

  -- Ensure that functions are correctly polymorphic when stored in cells
  , xType "def f = (fun x -> x) in (f 1) & (f ())"

  -- Ensure that constraints propogate into functions properly
  , xCont "(fun f -> f ()) (fun x -> [+] 1 x)"
  , xCont "(fun x -> [+] x 1) 'a'"

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
  , xNotC "(fun x -> [+] n 2)"
  , xPars "fun x -> case x of {`True a -> 1; `False a -> 0}"
          (A.Func idX
             (A.Case varX
                     [ A.Branch Nothing
                              (A.ChiLabel (labelName "True")
                                          (ident "a"))
                              (A.PrimInt 1)
                     , A.Branch Nothing
                              (A.ChiLabel (labelName "False")
                                          (ident "a"))
                              (A.PrimInt 0)
                     ]))
  ]
