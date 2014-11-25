{-# LANGUAGE LambdaCase #-}

{-|
  This module tests the LittleBang Parser.
-}
module Test.LittleBang.Syntax.Parser
( parserTests
) where

import Language.LittleBang.Ast as LB
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBangNested.Ast as TBN
import Test.HUnit

-- TODO: these parser tests are taken straight from TBN; add some LB stuff

-- |Constructs a positive parser test.  A positive test consists of a source
--  string and a predicate function which tests the resulting TExpression.
postest :: String -> String -> (LB.Expr -> Bool) -> Test
postest name src check = TestLabel name $ TestCase $
  let ans = lexLittleBang UnknownDocument src >>=
            parseLittleBang UnknownDocument
  in
  case ans of
    Left err -> assertFailure err
    Right ast ->
      assertBool
        (display $ text "AST did not match predicate function; produced:" </>
                   indent 2 (align $ vcat
                      [ text "Display:" <+> align (makeDoc ast)
                      , text "Show:   " <+> align (text $ show ast)
                      ])) $
        check ast
      
-- |Constructs a negative parser test.  A negative test will only succeed if it
--  lexes successfully but does not parse.
negtest :: String -> String -> Test
negtest name src = TestLabel name $ TestCase $
  case lexLittleBang UnknownDocument src of
    Left err -> assertFailure err
    Right tokens ->
      case parseLittleBang UnknownDocument tokens of
        Left _ -> return ()
        Right ast ->
          assertFailure $ display $
            text "Parse was expected to fail but produced an AST:" </>
            indent 2 (align $
              text "TExpression:" <+> text src <> line <>
              text "AST:       " <+> makeDoc ast)

-- |The parser tests to run.
parserTests :: Test
parserTests = TestList
  [ negtest "standalone label" "`A"
  , postest "simple let"
      "let x = 1 in x" 
      (\case
          TExprLet _ x1 (TExprValInt _ 1) (TExprVar _ x1') | x1 == x1' -> True
          _ -> False)
  , postest "arith op associativity"
      "a1 + b2 + c3"
      (\case
          TExprBinaryOp _
            (TExprBinaryOp _ (TExprVar _ _) (TBN.OpIntPlus _) (TExprVar _ _))
            (TBN.OpIntPlus _)
            (TExprVar _ _) -> True
          _ -> False)
  , postest "onion associativity"
      "x & 2 & `A 1"
      (\case
          TExprOnion _
            (TExprOnion _ (TExprVar _ _) (TExprValInt _ 2))
            (TExprLabelExp _ (LB.LabelName _ "A") (TExprValInt _ 1)) -> True
          _ -> False)
  , postest "nested label"
      "`A `B `C 1"
      (\case
          TExprLabelExp _ (LB.LabelName _ "A")
            (TExprLabelExp _ (LB.LabelName _ "B")
              (TExprLabelExp _ (LB.LabelName _ "C")
                (TExprValInt _ 1))) -> True
          _ -> False)
  , postest "application"
      "a(b=c)"
      (\case
          LExprAppl _ (TExprVar _ (LB.Ident _ "a"))
            [NamedArg _ (LB.Ident _ "b") (TExprVar _ (LB.Ident _ "c"))]
              -> True
          _ -> False)
  , postest "application precedence"
      "a + b & c(d)"
      (\case
          TExprBinaryOp _ (TExprVar _ _) (TBN.OpIntPlus _)
            (TExprOnion _ (TExprVar _ _)
              (LExprAppl _ (TExprVar _ _) [PositionalArg _ _])) -> True
          _ -> False)
  , postest "function parse"
      "fun a:int -> a"
      (\case
          LExprScape _ [Param _ (LB.Ident _ "a") (LB.PrimitivePattern _ LB.PrimInt)]
            (TExprVar _ (LB.Ident _ "a"))
              -> True
          _ -> False)
  , postest "pattern precedence"
      "fun a:(`A int * int) -> ()"
      (\case
          LExprScape _
            [Param _ (LB.Ident _ "a")
              (LB.ConjunctionPattern _
                (LB.LabelPattern _ (LB.LabelName _ "A")
                  (LB.PrimitivePattern _ LB.PrimInt))
                (LB.PrimitivePattern _ LB.PrimInt))
            ] (TExprValEmptyOnion _)
              -> True
          _ -> False)
  ]
