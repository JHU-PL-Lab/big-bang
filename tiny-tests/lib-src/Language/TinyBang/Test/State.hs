module Language.TinyBang.Test.State
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

idX = ident "x"
varX :: A.Expr
varX = inj $ A.Var idX

efour :: A.Expr
efour = inj $ A.PrimInt 4
four :: A.Value IA.Expr
four = A.VPrimInt 4
two :: A.Value IA.Expr
two = A.VPrimInt 2

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "State tests" $ TestList
  [ xPars "def x = 4 in x" $
          inj $ A.Def Nothing idX efour varX
  , xPars "x = 4 in x" $
          inj $ A.Assign idX efour varX
  , xPars "def x = 4 in x & 'a'" $
          inj $ A.Def Nothing idX efour $ inj $
            A.Onion varX $ inj $ A.PrimChar 'a'
  , xPars "x = 4 in x & 'a'" $
          inj $ A.Assign idX efour $ inj $
            A.Onion varX $ inj $ A.PrimChar 'a'
  , xPars "def x = 3 in x = 4 in x" $
          inj $ A.Def Nothing idX (inj $ A.PrimInt 3) $ inj $
            A.Assign idX efour varX

  -- Test evaluation of definition and assignment
  , xEval "def x = 4 in x" four
  , xNotC "x = 4 in x"
  , xEval "def x = 3 in x = 4 in x" four
  , xEval "def x = () in x = 4 in x" four
  , xEval "def x = () in (unit -> 4) x" four

  -- Test that def can be encoded with case.
  , xEval "(`Ref x -> x = 2 in x) `Ref 4" two

  -- The next two tests are contradictions due to flow insensitivity.
  , xCont "def x = () in x = 2 in (unit -> 4) x"
  , xCont "def x = () in x = 2 in (int -> 4) x"

  , xEval "def x = () in x = 2 in ((unit -> 2) & (int -> 4)) x" four

  -- TODO: add more unit tests for finality and immutability
  , xCont "(`Ref x -> x = x + 1 in x) `Ref final 5"
  , xCont "(`Ref x -> x = x + 1 in x) `Ref immut 5"
  , xCont "(`Ref `Ref x -> x = x + 1 in x) `Ref immut `Ref 5"
  ]