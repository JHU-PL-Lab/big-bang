module Language.TinyBang.Test.Parser
( tests
)
where

import Prelude hiding (pi)

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
  ( id_
  , idX
  , idY
  , idZ
  , lblA
  , lblB)
import Language.TinyBang.Test.ExpressionUtils
  ( pi
  , multiAppl
  , multiOnion
  , simpleScape
  , simpleDef
  , simpleAssign
  , simpleLabel
  , varX
  , varY
  , varZ
  , patAny
  , patInt
  , patFun
  , patChar
  , patUnit
  , false
  , identFuncX
  )
import Language.TinyBang.Test.SourceUtils
  ( tbCase
  )
import qualified Language.TinyBang.Config as Cfg

import qualified Language.TinyBang.Ast as A
import Utils.Language.Ast (astwrap)
import qualified Language.TinyBang.Types.Types as T (PrimitiveType(..))

projInt = A.ProjPrim T.PrimInt

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous parser tests" $ TestList
  [ fPars ""
  , fPars "(expr"
  , fPars "square x;"
  , fPars "case x of { int -> 3; }"
  , fPars "case x of {}"
  , fPars "def x"
  , fPars "def x in y"
  , fPars "x = y"
  , fPars "{ 1 }"
  , fPars "1 & x -> x"
  , fPars "f x -> x"

  , xPars "(((1)))" $ pi 1
  , xPars "any -> 0" $ simpleScape "_" (pi 0)
  , xPars "def x = 1 in x & x" $
          simpleDef "x" (pi 1) (multiOnion [varX, varX])
  , xPars "x -> x & x" $
          simpleScape "x" (multiOnion [varX, varX])
  , xPars "x -> x = 1 in x" $
          simpleScape "x" $ simpleAssign "x" (pi 1) varX
  , xPars "x = 1 in x -> x" $
          simpleAssign "x" (pi 1) $ simpleScape "x" varX
  , xPars "def x = 1 in x -> x" $
          simpleDef "x" (pi 1) $ simpleScape "x" varX
  , xPars "y -> x -> y & x" $
          simpleScape "y" $ simpleScape "x" $ multiOnion [varY, varX]
  , xPars "1 & 2 &- int & ()" $
          multiOnion [ astwrap $ A.OnionSub (multiOnion [pi 1, pi 2]) projInt
                     , astwrap $ A.PrimUnit]
  , xPars "1 & 2 &. int &- int" $ astwrap $
          A.OnionSub
            (astwrap $ A.OnionProj (multiOnion [pi 1, pi 2]) projInt)
            projInt
  , xPars "1 == 2 &- int" $ astwrap $
          A.OnionSub (astwrap $ A.EagerOp A.Equal (pi 1) (pi 2)) projInt
  , xPars "1 + 2 == 3" $ astwrap $
          A.EagerOp A.Equal
            (astwrap $ A.LazyOp A.Plus (pi 1) (pi 2))
            (pi 3)
  , xPars "1 + 2 == 2 + 1" $ astwrap $
          A.EagerOp A.Equal
            (astwrap $ A.LazyOp A.Plus (pi 1) (pi 2))
            (astwrap $ A.LazyOp A.Plus (pi 2) (pi 1))
  -- We decide that this shouldn't parse because of potential confusion
  , fPars "1 & x -> x"
--          multiOnion [pi 1, simpleScape "x" varX]
  , xPars "x -> x & 1" $
          simpleScape "x" $ multiOnion [varX, pi 1]
  , fPars "y x -> x"
--          multiAppl [varY, simpleScape "x" varX]
  , xPars "`A `B ()" $
          simpleLabel "A" $ simpleLabel "B" $ astwrap A.PrimUnit
  , xPars "`A (x -> x)" $
          simpleLabel "A" $ simpleScape "x" varX
  , xPars "`A x -> x" $ astwrap $
          A.Scape (A.Pattern id_ $ A.PatLabel lblA idX patAny) varX
  , xPars "`A 1 & `B 2" $
          multiOnion [simpleLabel "A" $ pi 1, simpleLabel "B" $ pi 2]
  , xPars "`A `B 1 & `C 2" $
          multiOnion [ simpleLabel "A" $ simpleLabel "B" $ pi 1
                     , simpleLabel "C" $ pi 2]
  , xPars "x:int -> x" $ astwrap $
          A.Scape (A.Pattern idX $ A.PatPrim T.PrimInt) varX

  , fPars "`A x & `B y -> x + y"
  , xPars "(`A x & `B y) -> x + y" $ astwrap $
          A.Scape (A.Pattern id_ $ A.PatOnion [ A.PatLabel lblA idX patAny
                                              , A.PatLabel lblB idY patAny])
                  (astwrap $ A.LazyOp A.Plus varX varY)
  , xPars "x y z" $
          multiAppl [varX, varY, varZ]
  , xPars "y == z x" $ astwrap $
          A.EagerOp A.Equal varY (multiAppl [varZ, varX])
  , xPars "z y == x" $ astwrap $
          A.EagerOp A.Equal (multiAppl [varZ, varY]) varX
  , xPars "z y == z x" $ astwrap $
          A.EagerOp A.Equal (multiAppl [varZ, varY]) (multiAppl [varZ, varX])
  , fPars "a + + b"
  , xPars "`A final 0" $ astwrap $
          A.Label lblA (Just A.Final) (pi 0)
  , fPars "x & y -> x"
  , xPars (tbCase "x"
           [ "int -> 5"
           , "char -> 'a'"
           , "unit -> ()"
           , "`A x -> `False ()"
           , "fun -> (x -> x)"])
          (multiAppl
           [ multiOnion $ reverse $ map astwrap
             [ A.Scape (A.Pattern id_ patInt) $ pi 5
             , A.Scape (A.Pattern id_ patChar) $ astwrap $ A.PrimChar 'a'
             , A.Scape (A.Pattern id_ patUnit) $ astwrap A.PrimUnit
             , A.Scape (A.Pattern id_ $ A.PatLabel lblA idX patAny) false
             , A.Scape (A.Pattern id_ patFun) $ identFuncX]
           , varX])
  ]
