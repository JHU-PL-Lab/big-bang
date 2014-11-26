{
{-# LANGUAGE GADTs #-}
module Language.TinyBangNested.Syntax.Parser
( parseTinyBangNested
, parseTinyBangNestedPattern
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax 
import Language.TinyBangNested.Ast
import Language.TinyBangNested.Syntax.Tokens
}

%name parseTokens
%monad { ParserM }
%tokentype { Token }
%error { parseError }

%token
  eof           { Token (SomeToken TokEOF $$) }
  'int'         { Token (SomeToken TokInt $$) } 
  'char'        { Token (SomeToken TokChar $$) }
  'ref'         { Token (SomeToken TokRef $$) }
  'let'         { Token (SomeToken TokLet $$) }
  'in'          { Token (SomeToken TokIn $$) }
  'getChar'     { Token (SomeToken TokGetChar $$) }
  'putChar'     { Token (SomeToken TokPutChar $$) }
  'fun'         { Token (SomeToken TokLambda $$) }
  '->'          { Token (SomeToken TokArrow $$) }
  '()'          { Token (SomeToken TokEmptyOnion $$) }
  '=='          { Token (SomeToken TokEq $$) }
  '<='          { Token (SomeToken TokLessEq $$) }
  '>='          { Token (SomeToken TokGreaterEq $$) }
  '<-'          { Token (SomeToken TokSet $$) }
  '&'           { Token (SomeToken TokOnion $$) }
  '='           { Token (SomeToken TokIs $$) }
  '('           { Token (SomeToken TokOpenParen $$) }
  ')'           { Token (SomeToken TokCloseParen $$) }
  '+'           { Token (SomeToken TokPlus $$) }
  '-'           { Token (SomeToken TokMinus $$) }
  '*'           { Token (SomeToken TokAsterisk $$) }
  '/'           { Token (SomeToken TokDiv $$) }
  '%'           { Token (SomeToken TokMod $$) }
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }
  litchar       { Token (SomeToken TokLitChar $$) }

%left LAM
%right 'in'
%nonassoc '<=' '>=' '=='
%right '<-'
%left '+' '-'
%left '*' '/' '%'
%left '&'
%right 'putChar'

%name parseProgram Program
%name parsePattern OnlyPattern

%%

Program :: { SPositional Expr }
  : Expr eof                { $1 }

OnlyPattern :: { SPositional Pattern }
  : Pattern eof             { $1 }

Expr :: { SPositional Expr }
  : 'let' Ident '=' Expr 'in' Expr
                            { oc3 $1 $> ExprLet $2 $4 $6 }
  | 'fun' Pattern '->' Expr %prec LAM
                            { oc2 $1 $> ExprScape $2 $4 }
  | Expr '+' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntPlus `oat` $2) $3 }
  | Expr '-' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntMinus `oat` $2) $3 }
  | Expr '*' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntMult `oat` $2) $3 }
  | Expr '/' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntDiv `oat` $2) $3 }
  | Expr '%' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntMod `oat` $2) $3 }
  | Expr '==' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntEq `oat` $2) $3 }
  | Expr '>=' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntGreaterEq `oat` $2) $3 }
  | Expr '<=' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntLessEq `oat` $2) $3 }
  | Expr '<-' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpSet `oat` $2) $3 }
  | Expr '&' Expr           { oc2 $1 $> ExprOnion $1 $3 }
  | ApplExpr                { $1 }
  | 'getChar'               { oc0 $1 $> ExprGetChar }
  | 'putChar' Expr          { oc1 $1 $> ExprPutChar $2 }

ApplExpr :: { SPositional Expr }
  : ApplExpr PrefixExpr     { oc2 $1 $> ExprAppl $1 $2 }
  | PrefixExpr              { $1 }

PrefixExpr :: { SPositional Expr }
  : Label PrefixExpr        { oc2 $1 $> ExprLabelExp $1 $2 }
  | 'ref' PrefixExpr        { oc1 $1 $> ExprRef $2 }
  | PrimaryExpr             { $1 }

PrimaryExpr :: { SPositional Expr }
  : Ident                   { oc1 $1 $> ExprVar $1 }
  | LiteralExpr             { $1 }
  | '(' Expr ')'            { $2 }

Ident :: { SPositional Ident }
  : ident                   { oc1 $1 $> Ident $1 }
  
Label :: { SPositional LabelName }
  : label                   { oc1 $1 $> LabelName $1 }

LiteralExpr :: { SPositional Expr }
  : '()'                    { oc0 $1 $> ExprValEmptyOnion }
  | litint                  { oc1 $1 $> ExprValInt $1 }
  | litchar                 { oc1 $1 $> ExprValChar $1 }

Pattern :: { SPositional Pattern }
  : Pattern '*' Pattern     { oc2 $1 $> ConjunctionPattern $1 $3 }
  | PrimaryPattern          { $1 }

PrimaryPattern :: {SPositional Pattern }
  : Label PrimaryPattern    { oc2 $1 $> LabelPattern $1 $2 }
  | 'ref' Ident             { oc1 $1 $> RefPattern (oc1S VariablePattern $2) }
  | PrimitiveType           { oc1 $1 $> PrimitivePattern $1 }
  | '()'                    { oc0 $1 $> EmptyPattern }
  | Ident                   { oc1 $1 $> VariablePattern $1 }
  | '(' Pattern ')'         { $2 }

PrimitiveType :: { SPositional PrimitiveType }
  : 'int'                   { PrimInt `at` $1  }
  | 'char'                  { PrimChar `at` $1  }

-- Generalizations of common grammar patterns.

many1(p)
  : p                       { fmap (:[]) $1 }
  | p many1(p)              { posOver $1 $2 $ posData $1 : posData $2 } 

many(p)
  : many1(p)                { forgetSpan $1 }
  |                         { vpos [] }

{

parseTinyBangNested :: SourceDocument -> [Token] -> Either String Expr
parseTinyBangNested _ toks =
    posData <$> parseProgram toks

parseTinyBangNestedPattern :: SourceDocument -> [Token] -> Either String Pattern
parseTinyBangNestedPattern _ toks =
    posData <$> parsePattern toks

}
