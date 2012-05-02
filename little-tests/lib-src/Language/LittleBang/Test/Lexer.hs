module Language.LittleBang.Test.Lexer
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.TinyBang.Config as Cfg

-- TODO: write a whitespace test using quickcheck
-- TODO: write show instance for list of tokens
-- TODO: write a test that a list of tokens that is shown lexes to the
-- original list
-- TODO: Write a lex-fail construct
-- TODO: test that [+] and the like fail to lex

lexesAs :: (String, Token) -> Test
lexesAs (str, tok) =
  xLexs str [tok]

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous lexer tests" $ TestList $
  [ xLexs "" []
  , xLexs " " []
  , xLexs "\t" []
  , xLexs "\n" []
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
  , xLexs "a&-`b"
          [ TokIdentifier "a"
          , TokOnionSub
          , TokLabelPrefix
          , TokIdentifier "b"
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
  ] ++ map lexesAs
    [  ("`"     , TokLabelPrefix)
    ,  ("&"     , TokOnionCons)
    ,  ("&-"    , TokOnionSub)
    ,  ("&."    , TokOnionProj)
    ,  ("\\"    , TokLambda)
    ,  ("fun"   , TokFun)
    ,  ("->"    , TokArrow)
    ,  ("case"  , TokCase)
    ,  ("of"    , TokOf)
    ,  ("int"   , TokInteger)
    ,  ("char"  , TokChar)
    ,  ("unit"  , TokUnit)
    ,  ("("     , TokOpenParen)
    ,  (")"     , TokCloseParen)
    ,  ("{"     , TokOpenBlock)
    ,  ("}"     , TokCloseBlock)
    ,  (";"     , TokSeparator)
    ,  (":"     , TokColon)
    ,  ("def"   , TokDef)
    ,  ("+"     , TokOpPlus)
    ,  ("-"     , TokOpMinus)
    ,  ("=="    , TokOpEquals)
    ,  ("<="    , TokOpLessEquals)
    ,  (">="    , TokOpGreaterEquals)
    ,  ("="     , TokEquals)
    ,  ("in"    , TokIn)
    ]
