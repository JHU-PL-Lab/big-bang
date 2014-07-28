{-# LANGUAGE LambdaCase #-}

{-|
  This module tests the TinyBangNested Parser.
-}
module Test.TinyBangNested.Syntax.Parser
( parserTests
) where

import Language.TinyBangNested.Ast
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax.Location
import Test.HUnit

-- |Constructs a positive parser test.  A positive test consists of a source
--  string and a predicate function which tests the resulting expression.
postest :: String -> String -> (Expr -> Bool) -> Test
postest name src check = TestLabel name $ TestCase $
  let ans = lexTinyBangNested UnknownDocument src >>=
            parseTinyBangNested UnknownDocument
  in
  case ans of
    Left err -> assertFailure err
    Right ast ->
      assertBool
        (display $ text "AST did not match predicate function; produced:" <>
                   line <> indent 2 (align $ text $ show ast)) $
        check ast
      
-- |Constructs a negative parser test.  A negative test will only succeed if it
--  lexes successfully but does not parse.
negtest :: String -> String -> Test
negtest name src = TestLabel name $ TestCase $
  case lexTinyBangNested UnknownDocument src of
    Left err -> assertFailure err
    Right tokens ->
      case parseTinyBangNested UnknownDocument tokens of
        Left _ -> return ()
        Right ast ->
          assertFailure $ display $
            text "Parse was expected to fail but produced an AST:" <> line <>
            indent 2 (align $
              text "Expression:" <+> text src <> line <>
              text "AST:       " <+> makeDoc ast)

-- |The parser tests to run.
parserTests :: Test
parserTests = TestList
  [ negtest "standalone label" "`A"
  , postest "simple let"
      "let x = 1 in x" 
      (\case
          ExprLet _ x1 (ExprValInt _ 1) (ExprVar _ x1') | x1 == x1' -> True
          _ -> False)
  , postest "arith op associativity"
      "a1 + b2 + c3"
      (\case
          ExprBinaryOp _
            (ExprBinaryOp _ (ExprVar _ _) (OpIntPlus _) (ExprVar _ _))
            (OpIntPlus _)
            (ExprVar _ _) -> True
          _ -> False)
  , postest "onion associativity"
      "x & 2 & `A 1"
      (\case
          ExprOnion _
            (ExprOnion _ (ExprVar _ _) (ExprValInt _ 2))
            (ExprLabelExp _ (LabelName _ "A") (ExprValInt _ 1)) -> True
          _ -> False)
  , postest "nested label"
      "`A `B `C 1"
      (\case
          ExprLabelExp _ (LabelName _ "A")
            (ExprLabelExp _ (LabelName _ "B")
              (ExprLabelExp _ (LabelName _ "C")
                (ExprValInt _ 1))) -> True
          _ -> False)
  , postest "application associativity"
      "a b c"
      (\case
          ExprAppl _ (ExprAppl _ (ExprVar _ _) (ExprVar _ _)) (ExprVar _ _) ->
            True
          _ -> False)
  , postest "application precedence"
      "a + b & c d"
      (\case
          ExprBinaryOp _ (ExprVar _ _) (OpIntPlus _)
            (ExprOnion _ (ExprVar _ _)
              (ExprAppl _ (ExprVar _ _) (ExprVar _ _))) -> True
          _ -> False)
  , postest "scape parse"
      "`A x -> x + x"
      (\case
          ExprScape _ (LabelPattern _ (LabelName _ "A") (VariablePattern _ v))
            (ExprBinaryOp _ (ExprVar _ v') (OpIntPlus _) (ExprVar _ v''))
              | v == v' && v == v'' -> True
          _ -> False)
  , postest "pattern precedence"
      "`A int & int -> ()"
      (\case
          ExprScape _
            (ConjunctionPattern _
              (LabelPattern _ (LabelName _ "A") (PrimitivePattern _ PrimInt))
              (PrimitivePattern _ PrimInt))
            _ -> True
          _ -> False)
  ]
