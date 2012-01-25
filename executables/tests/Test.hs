module Main where

import qualified Language.TinyBang.Render.PrettyPrintTest as TPP
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

type TinyBangCode = String
type Result = (A.Value, IntMap A.Value)

xEval :: (Display v, Evaluated v) => TinyBangCode -> v -> Test
xEval code expectedResult =
  label ~: TestCase $ case wrappedResult of
    EvalResult _ sOrF -> case sOrF of
                           EvalSuccess result ->
                             assertEqual ""
                                         (vmPair expectedResult)
                                         (vmPair result)
                           EvalFailure err -> assertFailure $
                                                "EvalFailure: " ++ display err
    _ -> assertFailure $
         "Expected evaluation to succeed but instead got " ++
         display wrappedResult
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce " ++ display expectedResult

xType :: TinyBangCode -> Test
xType code =
  label ~: TestCase $ case evalStringTop code of
    EvalResult _ _ -> assertSuccess
    result@_ -> assertFailure $
         "Expected closure to succeed but instead got " ++
         display result
  where label = show code ++
                " was expected to typecheck with no contradictions."

assertSuccess :: Assertion
assertSuccess = return ()

xCont :: TinyBangCode -> Test
xCont code =
  label ~: case result of
    Contradiction _ _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expression evaluated to " ++
         display result ++
         ", which did not produce a contradiction"
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xNotC :: TinyBangCode -> Test
xNotC code =
  label ~: case result of
    TypecheckFailure _ (TI.NotClosed _) _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected NotClosed but instead got " ++
         display result
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xLexs :: TinyBangCode -> [Token] -> Test
xLexs code expected =
  label ~: TestCase $ case L.lexTinyBang code of
    Left err -> assertFailure $ "Lexing failed with: " ++ err
    Right tokens -> assertEqual "" expected tokens
  where label = "Lexing " ++ show code

xPars :: TinyBangCode -> A.Expr -> Test
xPars code expected =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure err -> assertFailure $ "Parse failed: " ++ show err
    TypecheckFailure expr _ _ -> eq expr
    Contradiction expr _ -> eq expr
    EvalResult expr _ -> eq expr
  where label = "Parsing " ++ show code
        eq = assertEqual "" expected

fPars :: TinyBangCode -> Test
fPars code =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure _ -> assertSuccess
    TypecheckFailure expr _ _ -> failWith expr
    Contradiction expr _ -> failWith expr
    EvalResult expr _ -> failWith expr
  where label = "Parsing " ++ show code
        failWith expr = assertFailure $ "Parse succeeded with " ++ display expr

multiAppl :: [A.Expr] -> A.Expr
multiAppl [] = error "multiAppl used on empty list"
multiAppl xs = foldl1 A.Appl xs

srcSummate :: TinyBangCode
srcSummate = "fun this -> fun x -> case (equal x 0) of { `True z -> 0 ; `False z -> plus x (this (minus x 1))}"
srcMultiAppl :: [TinyBangCode] -> TinyBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

srcGreaterOrLessUtil =
 "fun this -> fun x -> fun y -> fun z ->"++
     "case equal (minus x y) z of {"++
          "`True junk -> `GreaterThan () ;"++
          "`False junk ->"++
                 "case equal (minus y x) z of {"++
                      "`True junk -> `LessThan () ;"++
                      "`False junk -> this x y (plus z 1) }}"

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


tests = TestList $ [TPP.tests] ++
-- Test proper handling of an arbitrary positive and negative integer literal
  [ xLexs "1234567890"
          [TokIntegerLiteral 1234567890]
  , xLexs "-1234567890"
          [TokIntegerLiteral (-1234567890)]
  , xPars "1234567890" $
          A.PrimInt 1234567890
  , xPars "-1234567890" $
          A.PrimInt (-1234567890)
  , xType "1234567890"
  , xType "-1234567890"
  , xEval "1234567890" $
          A.VPrimInt 1234567890
  , xEval "-1234567890" $
          A.VPrimInt (-1234567890)
-- Test parsing of definition and assignment
  , xPars "def x = 4 in x" $
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
  , xEval "def x = 4 in x" $
          four
  , xNotC "x = 4 in x"
  , xEval "def x = 3 in x = 4 in x" four
  , xEval "def x = () in x = 4 in x" four
  , xEval "def x = () in case x of { unit -> 4 }" four
  , xCont "def x = () in x = 2 in case x of { unit -> 4 }"
  , xCont "def x = () in x = 2 in case x of { int -> 4 }"
  , xEval "def x = () in x = 2 in case x of { unit -> 2 ; int -> 4 }" four
-- Test proper handling of arbitrary ASCII characters
  , xType "'x'"
  , xEval "'a'" $
          A.VPrimChar 'a'
  , xEval "'A'" $
          A.VPrimChar 'A'
  , xEval "'z'" $
          A.VPrimChar 'z'
  , xEval "'Z'" $
          A.VPrimChar 'Z'
-- Test proper handling of arbitrary Unicode characters
  , xEval "'∀'" $
          A.VPrimChar '∀'
  , xEval "'∃'" $
          A.VPrimChar '∃'
  , xEval "'λ'" $
          A.VPrimChar 'λ'
  , xEval "'◈'" $
          A.VPrimChar '◈'
  , xEval "'☺'" $
          A.VPrimChar '☺'
  , xEval "'⤳'" $
          A.VPrimChar '⤳'
  , xEval "'☃'" $
          A.VPrimChar '☃'
-- Test proper handling of whitespace characters
  , xEval "' '" $
          A.VPrimChar ' '
  , xEval "'\t'" $
          A.VPrimChar '\t'
-- Test parse, typechecking, and evaluation of some simple values
  , xPars "()"
          A.PrimUnit
  , xPars "`True ()" $
          etrue
  , xPars "`False ()" $
          efalse
  , xPars "(\\x -> x)" $
          exIdent
  , xType "`A ()"
  , xType "()"
  , xEval "()"
          A.VPrimUnit
  , xEval "`True ()"
          true
  , xEval "`False ()"
          false
  , xEval "(\\x -> x)"
          xIdent
-- Test evaluation of some onions
  , xEval "`A 1 & `B 1" $
          ( A.VOnion (A.VLabel (labelName "A") 0) (A.VLabel (labelName "B") 1)
          , IntMap.fromList $ zip [0, 1] $ map A.VPrimInt $ repeat 1)
-- Test parse and evaluation of some simple arithmetic applications
-- TODO: uncomment when updated to include plus, etc.
--   , xPars "plus 2 2" $
--           multiAppl $ [A.Var (ident "plus"), etwo, etwo]
--   , xType "plus 1 2"
--   , xType "minus 1 2"
--   , xType "plus (minus (plus 1 2) 3) (plus (-2) (minus 4 0))"
--   , xEval "(fun x -> plus x x) 2"
--           four
--   , xEval "(\\x -> plus x x) 2"
--           four
--   , xEval "plus 2 2"
--           four
--   , xEval "minus 2 2"
--           zero
--   , xEval "minus 2 -2"
--           four
--   , xType "(fun x -> plus x 1) 1"
-- -- Test that arithmetic expressions on non-numeric literals fail to typecheck
--   , xCont "plus 1 'a'"
--   , xCont "plus 1 ()"
--   , xCont "plus 'a' 'a'"
--   , xCont "plus () ()"
--   , xCont "plus 2 'x'"
--   , xCont "plus 1 (fun x -> x)"
--   , xCont "minus 1 'a'"
--   , xCont "minus 1 ()"
--   , xCont "minus 'a' 'a'"
--   , xCont "minus () ()"
--   , xCont "(fun x -> plus x 1) 'a'"
-- -- Test evaluation of compound arithmetic application
--   , xEval "plus (minus 1 -1) (minus 1 -1)"
--           four
-- -- Test parse, typecheck, and evaluation of some higher order applications
--   , xPars "(fun x -> x)\n(fun x -> x)" $
--           A.Appl exIdent exIdent
--   , xEval "(fun x -> x)\n(fun x -> x)"
--           xIdent
--   , xType "(fun x -> x)"
--   , xType "(fun x -> x) (fun x -> x)"
--   , xType srcY
--   , xType (srcMultiAppl [srcY, srcSummate, "5"])
--   , xType (srcMultiAppl [srcGreaterOrLess, "4", "4"])
--   , xCont (srcMultiAppl [srcGreaterOrLess, "`A 4", "4"])
--   , xCont (srcMultiAppl [srcGreaterOrLess, "'a'", "4"])
--   , xType (srcMultiAppl [srcGreaterOrLess, "'a'"])
--   , xType "plus (2 & 'b') 2"
--   , xCont "plus (`True () & 'z') 2"
--   , xType "plus (2 & 'x') ('y' & 2)"
--   , xType "plus (2 & ('a' & ())) ((2 & 'b') & ())"
--   , xType "plus (1 & ('a' & ())) ('a' & (1 & ()))"
--   , xNotC "(fun x -> plus n 2)"
  , xNotC "case x of {int -> 0; char -> 'a'}"
  , xNotC "x"
-- Test evaluation of some recursive arithmetic evaluations
-- TODO: uncomment when we define equal
--  , xEval (srcMultiAppl [srcY, srcSummate, "5"]) $
--          A.VPrimInt 15
-- , xCont (srcMultiAppl [srcY, srcGreaterOrLess, "4", "4"])
-- , xEval (srcMultiAppl [srcGreaterOrLess, "4", "4"]) $
--         lblEq
-- , xEval (srcMultiAppl [srcGreaterOrLess, "0", "4"]) $
--         lblLt
-- , xEval (srcMultiAppl [srcGreaterOrLess, "4", "0"]) $
--         lblGt
-- Test parsing of nonterminating function
  , xPars "(fun x -> x x) (fun x -> x x)" $
          A.Appl (A.exprFromValue xomega) (A.exprFromValue xomega)
  , xType "(fun x -> x x) (fun x -> x x)"
-- Test typechecking of some pathological functions
  , xType $ srcMultiAppl [srcY, "fun this -> fun x -> this (`A x & `B x)"]
  , xType $ srcMultiAppl [srcY, "fun this -> fun x -> this (`A x & `B x)", "0"]
  , xType $ srcMultiAppl [srcY, "fun this -> fun x -> this (`A x & `B x)", "()"]
  , xType $ srcMultiAppl [srcY, "fun this -> fun x -> this (`A x & `B x)", "`A () & `B ()"]
  , xType $ srcMultiAppl [srcY, "fun this -> fun x -> this (`A x & `B x)", srcY]
-- -- Test simple equalities
-- TODO: uncomment when we have equal
--   , xType "equal 1 1"
--   , xEval "equal 1 1"
--           true
--   , xEval "equal 0 1"
--           false
--   , xEval "equal 0 (minus (minus (plus 1 1) 1) 1)"
--           true
--   , xType "equal 'a' 'a'"
--   , xEval "equal 'a' 'a'"
--           true
--   , xEval "equal 'a' 'A'"
--           false
--   , xEval "equal `True () `True ()"
--           true
--   , xEval "equal `A 1 `A 1"
--           true
--   , xType "equal () ()"
--   , xEval "equal () ()"
--           true
-- -- TODO: make these tests pass by implementing the equality constraint
--   , xCont "equal `A 1 `B 1"
--   , xCont "equal `True () `False ()"
--   , xCont "equal 1 'a'"
--   , xCont "equal 1 ()"
--   , xCont "equal (fun x -> x) (fun y -> y)"
--   , xType "(fun f -> equal f f) (fun x -> x)"
--   , xEval "(fun f -> equal f f) (fun x -> x)" $
--           true
-- -- Test equality evaluations on onions
-- -- TODO: Make all of these pass
--   , xEval "equal (1 & 'a') ('a' & 1)"
--           true
--   , xEval "equal ('a' & 1) (1 & 'a')"
--           true
--   , xEval "equal (1 & 'a') (1 & 'z')"
--           false
--   , xEval "equal (1 & 'a') (0 & 'a')"
--           false
--   , xEval "equal (1 & 2) (2 & 1)"
--           false
-- Test case projection
  , xEval "case `A 5 & `A \'a\' of {`A x -> x}" $
-- This is no longer true
--          (A.VOnion (A.VPrimInt 5) (A.VPrimChar 'a'))
          A.VPrimChar 'a'
  , xEval "case `A \'a\' & `A 5 of {`A x -> x}" $
          A.VPrimInt 5
  , xEval "case 'a' of {char -> 0}"
          zero
  , xEval "case 1234567890 of {int -> 0}"
          zero
  , xEval "case (fun x -> x) of {fun -> 0}"
          zero
  , xEval "case (\\x -> x) of {fun -> 0}"
          zero
  , xEval "case () of {unit -> 0}"
          zero
  , xEval "case `Test () of {`Test a -> 0}"
          zero
  , xEval "case `B 2 of {`A x -> 0; `B y -> 1}"
          one
  , xEval "case `A 0 of {`A n -> n}"
          zero
  , xEval "case `A 1 of {`A a -> 1; `A n -> 0}"
          one
  , xType "case `A 5 of { `A x -> x }"
-- Test that implicit projection from onions fails
-- TODO: uncomment when we have plus
--  , xEval "plus (1 & 'a') ('a' & 1 & ())"
--          two
  , xCont "(1 & (fun x -> x)) 1"
-- Test that application requires that the first argument be a function
  , xCont "1 'x'"
-- Test that incomplete case statements result in contradiction
  , xCont "case 1 of {char -> 0}"
  , xCont "case `A 4 of { `B x -> x }"
-- Test that symbols lex to what we expect
  , xLexs "\\"
          [TokLambda]
  , xLexs "->"
          [TokArrow]
  , xLexs "`"
          [TokLabelPrefix]
  , xLexs "fun"
          [TokFun]
  , xLexs "case"
          [TokCase]
  , xLexs "of"
          [TokOf]
  , xLexs "int"
          [TokInteger]
  , xLexs "char"
          [TokChar]
  , xLexs "&"
          [TokOnionCons]
  , xLexs "("
          [TokOpenParen]
  , xLexs ")"
          [TokCloseParen]
  , xLexs "{"
          [TokOpenBlock]
  , xLexs "}"
          [TokCloseBlock]
  , xLexs ";"
          [TokSeparator]
-- Test that some arbitrary symbol strings lex correctly
  , xLexs "'s''t''r''i''n''g'" $
          map TokCharLiteral "string"
  , xLexs "map (\\x -> x) (1;2;3)"
          [ TokIdentifier "map"
          , TokOpenParen
          , TokLambda
          , TokIdentifier "x"
          , TokArrow
          , TokIdentifier "x"
          , TokCloseParen
          , TokOpenParen
          , TokIntegerLiteral 1
          , TokSeparator
          , TokIntegerLiteral 2
          , TokSeparator
          , TokIntegerLiteral 3
          , TokCloseParen
          ]
  , xLexs "fun double x -> plus x x"
          [ TokFun
          , TokIdentifier "double"
          , TokIdentifier "x"
          , TokArrow
          , TokIdentifier "plus"
          , TokIdentifier "x"
          , TokIdentifier "x"
          ]
  , xLexs "fun abs x -> if x GTE 0 then x else negative x"
          [ TokFun
          , TokIdentifier "abs"
          , TokIdentifier "x"
          , TokArrow
          , TokIdentifier "if"
          , TokIdentifier "x"
          , TokIdentifier "GTE"
          , TokIntegerLiteral 0
          , TokIdentifier "then"
          , TokIdentifier "x"
          , TokIdentifier "else"
          , TokIdentifier "negative"
          , TokIdentifier "x"
          ]
  , xLexs "case val of\n x -> x"
          [ TokCase
          , TokIdentifier "val"
          , TokOf
          , TokIdentifier "x"
          , TokArrow
          , TokIdentifier "x"
          ]
  , xLexs "{`(int & unit} & char)"
          [ TokOpenBlock
          , TokLabelPrefix
          , TokOpenParen
          , TokInteger
          , TokOnionCons
          , TokUnit
          , TokCloseBlock
          , TokOnionCons
          , TokChar
          , TokCloseParen
          ]
  , xLexs "(\\x->x)"
          [ TokOpenParen
          , TokLambda
          , TokIdentifier "x"
          , TokArrow
          , TokIdentifier "x"
          , TokCloseParen
          ]
  , xLexs "a&(b&c)"
          [ TokIdentifier "a"
          , TokOnionCons
          , TokOpenParen
          , TokIdentifier "b"
          , TokOnionCons
          , TokIdentifier "c"
          , TokCloseParen
          ]
  , xLexs "`True()"
          [ TokLabelPrefix
          , TokIdentifier "True"
          , TokOpenParen
          , TokCloseParen
          ]
  , xLexs "(1;xyz;{\'c\';)}"
          [ TokOpenParen
          , TokIntegerLiteral 1
          , TokSeparator
          , TokIdentifier "xyz"
          , TokSeparator
          , TokOpenBlock
          , TokCharLiteral 'c'
          , TokSeparator
          , TokCloseParen
          , TokCloseBlock
          ]
-- Test that quotes in identifiers lex correctly
  , xLexs "1\'c\'xyz"
          [ TokIntegerLiteral 1
          , TokCharLiteral 'c'
          , TokIdentifier "xyz"
          ]
  , xLexs "1xyz\'c\'"
          [ TokIntegerLiteral 1
          , TokIdentifier "xyz\'c\'"
          ]
  , xLexs "xyz\'c\'1"
          [TokIdentifier "xyz\'c\'1"]
  , xLexs "xyz1\'c\'"
          [TokIdentifier "xyz1\'c\'"]
-- Test that whitespace is correctly ignored
  , xLexs ""
          []
  , xLexs " "
          []
  , xLexs "\t"
          []
  , xLexs "\n"
          []
-- Test that negative zero lexs to zero
  , xLexs "-0"
          [TokIntegerLiteral 0]
-- Test that a few expressions functions parse correctly
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
  , xPars "'s''t''r''i''n''g'" $
          multiAppl [ (A.PrimChar 's')
                    , (A.PrimChar 't')
                    , (A.PrimChar 'r')
                    , (A.PrimChar 'i')
                    , (A.PrimChar 'n')
                    , (A.PrimChar 'g')
                    ]
  , xPars "(1 & ('x' & (\\x -> x)))"
          (A.Onion (A.PrimInt 1)
                   (A.Onion (A.PrimChar 'x')
                   (A.Func idX varX)))
  , xPars "case x of {\
                      \    int -> 5;\
                      \    char -> 'a';\
                      \    unit -> ();\
                      \    `True a -> `False ();\
                      \    fun -> (\\x -> x)}" $
          A.Case varX
            [ A.Branch Nothing (A.ChiPrim T.PrimInt) $ A.PrimInt 5
            , A.Branch Nothing (A.ChiPrim T.PrimChar) $ A.PrimChar 'a'
            , A.Branch Nothing (A.ChiPrim T.PrimUnit) A.PrimUnit
            , A.Branch Nothing (A.ChiLabel (labelName "True") (ident "a")) efalse
            , A.Branch Nothing A.ChiFun $ A.Func (ident "x") varX
            ]
-- Test some simple parse failures
  , fPars ""
  , fPars "(expr"
  , fPars "square x;"
  , fPars "case x of { int -> 3; }"
  , fPars "case x of {}"
  ]


main :: IO Counts
main = runTestTT tests
