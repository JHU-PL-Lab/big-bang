module Language.LittleBang.Test.State
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
import Language.LittleBang.Test.ExpressionUtils
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

efour :: LA.Expr
efour = inj $ TA.PrimInt 4
four :: TA.Value IA.Expr
four = TA.VPrimInt 4
two :: TA.Value IA.Expr
two = TA.VPrimInt 2

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "State tests" $ TestList
  [ xPars "def x = 4 in x" $
          inj $ TA.Def Nothing idX efour varX
  , xPars "x = 4 in x" $
          inj $ TA.Assign idX efour varX
  , xPars "def x = 4 in x & 'a'" $
          inj $ TA.Def Nothing idX efour $ inj $
            LA.Onion varX $ inj $ TA.PrimChar 'a'
  , xPars "x = 4 in x & 'a'" $
          inj $ TA.Assign idX efour $ inj $
            LA.Onion varX $ inj $ TA.PrimChar 'a'
  , xPars "def x = 3 in x = 4 in x" $
          inj $ TA.Def Nothing idX (inj $ TA.PrimInt 3) $ inj $
            TA.Assign idX efour varX

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

  -- TODO: add unit tests for finality and immutability
  ]