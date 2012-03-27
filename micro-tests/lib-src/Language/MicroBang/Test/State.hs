module Language.MicroBang.Test.State
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

idX = ident "x"
varX = A.Var idX

efour = A.PrimInt 4
four = A.VPrimInt 4
two = A.VPrimInt 2

tests :: (?debug :: Bool) => Test
tests = TestLabel "State tests" $ TestList
  [ xPars "def x = 4 in x" $
          A.Def idX efour varX
  , xPars "x = 4 in x" $
          A.Assign (A.AIdent idX) efour varX
  , xPars "def x = 4 in x & 'a'" $
          A.Def idX efour $ A.Onion varX (A.PrimChar 'a')
  , xPars "x = 4 in x & 'a'" $
          A.Assign (A.AIdent idX) efour $ A.Onion varX (A.PrimChar 'a')
  , xPars "def x = 3 in x = 4 in x" $
          A.Def idX (A.PrimInt 3) $ A.Assign (A.AIdent idX) efour varX

  -- Test evaluation of definition and assignment
  , xEval "def x = 4 in x" four
  , xNotC "x = 4 in x"
  , xEval "def x = 3 in x = 4 in x" four
  , xEval "def x = () in x = 4 in x" four
  , xEval "def x = () in case x of { unit -> 4 }" four

  -- Test that def can be encoded with case.
  , xEval "case `Ref 4 of {`Ref x -> x = 2 in x}" two

  -- The next two tests are contradictions due to flow insensitivity.
  , xCont "def x = () in x = 2 in case x of { unit -> 4 }"
  , xCont "def x = () in x = 2 in case x of { int -> 4 }"
  , xEval "def x = () in x = 2 in case x of { unit -> 2 ; int -> 4 }" four
  ]