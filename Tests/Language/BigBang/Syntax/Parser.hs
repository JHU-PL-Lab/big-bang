module Tests.Language.BigBang.Syntax.Parser 
( testCases
) where

import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Syntax.Lexer
import Language.BigBang.Syntax.Parser
import Test.HUnit hiding (Label)
import Control.Exception

edgeCases = TestList [testParseEmptyString]
 
testParseEmptyString = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "")
      assertFailure "Input of \"\" should throw a parse error"

simpleCases = TestList [testParseInt, testLambdaExpr, testPerverseFunction, testFakeString, testTernaryOnion, testFakeBool, testIgnoreNewLines, testCaseOfBlock]
testParseInt = TestCase $ assertEqual
  "Input of 1234567890 should return PrimInt 1234567890"
  (PrimInt 1234567890)
  (parseBigBang $ lexBigBang "1234567890")

testLambdaExpr = TestCase $ assertEqual
  "Identity function: (\\x -> x)"
  (Func (ident "x") (Var (ident "x")))
  (parseBigBang $ lexBigBang "(\\x -> x)")

testPerverseFunction = TestCase $ assertEqual
  "(fun x -> x x) (fun x -> x x)"
  (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))) 
  (parseBigBang $ lexBigBang "(fun x -> x x) (fun x -> x x)")

testFakeString = TestCase $ assertEqual
  "Testing \"fake\" strings"
  (Appl (Appl (Appl (Appl (Appl (PrimChar 's') (PrimChar 't')) (PrimChar 'r')) (PrimChar 'i')) (PrimChar 'n')) (PrimChar 'g')) 
  (parseBigBang $ lexBigBang "'s''t''r''i''n''g'")

testTernaryOnion = TestCase $ assertEqual
  "Testing ternary onion"
  (Onion (PrimInt 1) (Onion (PrimChar 'x') (Func (ident "x") (Var (ident "x")))))
  (parseBigBang $ lexBigBang "(1 & ('x' & (\\x -> x)))")

testFakeBool = TestCase $ assertEqual
  "Testing \"fake\" boolean"
  (Label (labelName "true") PrimUnit)
  (parseBigBang $ lexBigBang "`true ()")


testCaseOfBlock = TestCase $ assertEqual
  "Testing case...of block"
  (Case (Var (ident "x")) [(ChiPrim T.PrimInt, PrimInt 5), (ChiPrim T.PrimChar, PrimChar 'a'), (ChiOnion (ident "a") (ident "b"), Label (labelName "True") PrimUnit), (ChiPrim T.PrimUnit, PrimUnit), (ChiLabel (labelName "True") (ident "a"), Label (labelName "False") PrimUnit), (ChiFun, Func (ident "x") (Var (ident "x")))])
  (parseBigBang $ lexBigBang "case x of {\nint -> 5;\nchar -> \'a\';\na&b -> `True ();\nunit -> ();\n`True a -> `False ();fun -> (\\x -> x)}")

testIgnoreNewLines = TestCase $ assertEqual
  "Test if parser ignores newlines correctly"
  (Appl (Func (ident "x") (Var (ident "x"))) (Func (ident "x") (Var (ident "x"))))
  (parseBigBang $ lexBigBang "(\\x->x)\n(\\x->x)")

testCases = TestList [edgeCases, simpleCases]
