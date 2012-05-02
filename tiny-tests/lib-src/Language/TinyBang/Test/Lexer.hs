module Language.TinyBang.Test.Lexer
( tests
)
where

import Language.TinyBang.Syntax.Lexer (SourceLocation)
import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Config as Cfg

-- TODO: write a whitespace test using quickcheck
-- TODO: write show instance for list of tokens
-- TODO: write a test that a list of tokens that is shown lexes to the
-- original list
-- TODO: Write a lex-fail construct
-- TODO: test that [+] and the like fail to lex

lexesAs :: (String, SourceLocation -> Token) -> Test
lexesAs (str, tok) =
  xLexs str [tok]

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous lexer tests" $ TestList $
  [ xLexs "" []
  , xLexs " " []
  , xLexs "\t" []
  , xLexs "\n" []
  , xLexs "'s''t''r''i''n''g'" $
          map (flip TokCharLiteral) "string"
  , xLexs "map (\\x -> x) (1;2;3)"
          [ (flip TokIdentifier) "map"
          , TokOpenParen
          , TokLambda
          , flip TokIdentifier "x"
          , TokArrow
          , flip TokIdentifier "x"
          , TokCloseParen
          , TokOpenParen
          , flip TokIntegerLiteral 1
          , TokSeparator
          , flip TokIntegerLiteral 2
          , TokSeparator
          , flip TokIntegerLiteral 3
          , TokCloseParen
          ]
  , xLexs "fun double x -> plus x x"
          [ TokFun
          , flip TokIdentifier "double"
          , flip TokIdentifier "x"
          , TokArrow
          , flip TokIdentifier "plus"
          , flip TokIdentifier "x"
          , flip TokIdentifier "x"
          ]
  , xLexs "fun abs x -> if x GTE 0 then x else negative x"
          [ TokFun
          , flip TokIdentifier "abs"
          , flip TokIdentifier "x"
          , TokArrow
          , flip TokIdentifier "if"
          , flip TokIdentifier "x"
          , flip TokIdentifier "GTE"
          , flip TokIntegerLiteral 0
          , flip TokIdentifier "then"
          , flip TokIdentifier "x"
          , flip TokIdentifier "else"
          , flip TokIdentifier "negative"
          , flip TokIdentifier "x"
          ]
  , xLexs "case val of\n x -> x"
          [ TokCase
          , flip TokIdentifier "val"
          , TokOf
          , flip TokIdentifier "x"
          , TokArrow
          , flip TokIdentifier "x"
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
          , flip TokIdentifier "x"
          , TokArrow
          , flip TokIdentifier "x"
          , TokCloseParen
          ]
  , xLexs "a&(b&c)"
          [ flip TokIdentifier "a"
          , TokOnionCons
          , TokOpenParen
          , flip TokIdentifier "b"
          , TokOnionCons
          , flip TokIdentifier "c"
          , TokCloseParen
          ]
  , xLexs "a&-`b"
          [ flip TokIdentifier "a"
          , TokOnionSub
          , TokLabelPrefix
          , flip TokIdentifier "b"
          ]
  , xLexs "`True()"
          [ TokLabelPrefix
          , flip TokIdentifier "True"
          , TokOpenParen
          , TokCloseParen
          ]
  , xLexs "(1;xyz;{\'c\';)}"
          [ TokOpenParen
          , flip TokIntegerLiteral 1
          , TokSeparator
          , flip TokIdentifier "xyz"
          , TokSeparator
          , TokOpenBlock
          , flip TokCharLiteral 'c'
          , TokSeparator
          , TokCloseParen
          , TokCloseBlock
          ]
-- Test that quotes in identifiers lex correctly
  , xLexs "1\'c\'xyz"
          [ flip TokIntegerLiteral 1
          , flip TokCharLiteral 'c'
          , flip TokIdentifier "xyz"
          ]
  , xLexs "1xyz\'c\'"
          [ flip TokIntegerLiteral 1
          , flip TokIdentifier "xyz\'c\'"
          ]
  , xLexs "xyz\'c\'1"
          [flip TokIdentifier "xyz\'c\'1"]
  , xLexs "xyz1\'c\'"
          [flip TokIdentifier "xyz1\'c\'"]
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
