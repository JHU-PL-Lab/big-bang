module Main where

--import qualified Language.TinyBang.Render.PrettyPrintTest as TPP
import Language.TinyBang.Interpreter.SourceInterpreter
import Test.HUnit
import Language.TinyBang.Render.Display (display, Display)
import qualified Language.TinyBang.Syntax.Lexer as L
import Language.TinyBang.Syntax.Lexer (Token(..))
import qualified Language.TinyBang.Types.TypeInference as TI
import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Ast (vmPair, Evaluated)
import Language.TinyBang.Types.UtilTypes (labelName, ident, Ident)
import qualified Language.TinyBang.Types.Types as T
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- TODO: TH to do appropriate qualified imports and runs.
import qualified Language.TinyBang.Test.Onions as Onions
import qualified Language.TinyBang.Test.Functions as Functions
import qualified Language.TinyBang.Test.Binders as Binders
import qualified Language.TinyBang.Test.Case as Case
import qualified Language.TinyBang.Test.EagerOps as EagerOps
import qualified Language.TinyBang.Test.LazyOps as LazyOps
import qualified Language.TinyBang.Test.Lexer as Lexer
import qualified Language.TinyBang.Test.Misc as Misc
import qualified Language.TinyBang.Test.OnionSubtraction as OnionSubtraction
import qualified Language.TinyBang.Test.Parser as Parser
import qualified Language.TinyBang.Test.Peano as Peano
import qualified Language.TinyBang.Test.Primitive.Char as Primitive.Char
import qualified Language.TinyBang.Test.Primitive.Int as Primitive.Int
import qualified Language.TinyBang.Test.Primitive.Unit as Primitive.Unit
import qualified Language.TinyBang.Test.Projection as Projection
import qualified Language.TinyBang.Test.State as State

tests =
  [ Onions.tests
  , Functions.tests
  , Binders.tests
  , Case.tests
  , EagerOps.tests
  , LazyOps.tests
  , Lexer.tests
  , Misc.tests
  , OnionSubtraction.tests
  , Parser.tests
  , Peano.tests
  , Primitive.Char.tests
  , Primitive.Int.tests
  , Primitive.Unit.tests
  , Projection.tests
  , State.tests
  ]

main = runTestTT $ TestList tests
{-
multiAppl :: [A.Expr] -> A.Expr
multiAppl [] = error "multiAppl used on empty list"
multiAppl xs = foldl1 A.Appl xs

srcSummate :: TinyBangCode
srcSummate = "fun this -> fun x -> case ([=] x 0) of { `True z -> 0 ; `False z -> [+] x (this ([-] x 1))}"

srcGreaterOrLessUtil :: TinyBangCode
srcGreaterOrLessUtil =
 "fun this -> fun x -> fun y -> fun z ->"++
     "case equal ([-] x y) z of {"++
          "`True junk -> `GreaterThan () ;"++
          "`False junk ->"++
                 "case [=] (minus y x) z of {"++
                      "`True junk -> `LessThan () ;"++
                      "`False junk -> this x y ([+] z 1) }}"

srcGreaterOrLess :: TinyBangCode
srcGreaterOrLess =
 "fun x -> fun y ->"++
     "case equal x y of {"++
          "`True junk -> `EqualTo () ;"++
           "`False junk -> "
           ++ srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"]
           ++ "}"

srcY  :: TinyBangCode
srcY  = "fun body -> (fun f -> fun arg -> f f arg) (fun this -> fun arg -> body (this this) arg)"

true  :: Result
true  = (A.VLabel (labelName "True") 0, IntMap.singleton 0 A.VPrimUnit)

etrue :: A.Expr
etrue = A.Label (labelName "True") A.PrimUnit

false :: Result
false = (A.VLabel (labelName "False") 0, IntMap.singleton 0 A.VPrimUnit)

efalse :: A.Expr
efalse = A.Label (labelName "False") A.PrimUnit

zero  :: A.Value
zero  = A.VPrimInt 0

one   :: A.Value
one   = A.VPrimInt 1

two   :: A.Value
two   = A.VPrimInt 2

etwo :: A.Expr
etwo = A.exprFromValue two

four  :: A.Value
four  = A.VPrimInt 4

efour :: A.Expr
efour = A.exprFromValue four

varX :: A.Expr
varX = A.Var $ idX

idX :: Ident
idX = ident "x"

xIdent :: A.Value
xIdent = A.VFunc idX varX

exIdent :: A.Expr
exIdent = A.exprFromValue xIdent

xomega :: A.Value
xomega = A.VFunc idX (A.Appl varX varX)

lblEq, lblLt, lblGt :: Result
lblEq = (A.VLabel (labelName "EqualTo") 0, IntMap.singleton 0 A.VPrimUnit)
lblLt = (A.VLabel (labelName "LessThan") 0, IntMap.singleton 0 A.VPrimUnit)
lblGt = (A.VLabel (labelName "GreaterThan") 0, IntMap.singleton 0 A.VPrimUnit)


-- TODO: Modularize the test cases a lot more than this.
-- For the moment, this "module" will just sit here.
-- TODO: end quasi-Peano module

-- tests = TestList $ [TPP.tests] ++ peanoTests ++
--   [
--   ]

-- main :: IO Counts
-- main = runTestTT tests
-}