{
{-# LANGUAGE GADTs #-}
module Language.TinyBangNested.Syntax.Parser
( parseTinyBangNested
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
  'int'         { Token (SomeToken TokInt $$) } 
  'ref'         { Token (SomeToken TokRef $$) }
  'let'         { Token (SomeToken TokLet $$) }
  'in'          { Token (SomeToken TokIn $$) }
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
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }

%left LAM
%right 'in'
%nonassoc '<=' '>=' '=='
%right '<-'
%left '+' '-'
%left '&'

%%

Program :: { SPositional Expr }
  : Expr                    { $1 }

Expr :: { SPositional Expr }
  : 'let' Var '=' Expr 'in' Expr
                            { oc3 $1 $> ExprLet $2 $4 $6 }
  | 'fun' Pattern '->' Expr %prec LAM
                            { oc2 $1 $> ExprScape $2 $4 }
  | Expr '+' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntPlus `oat` $2) $3 }
  | Expr '-' Expr           { oc3 $1 $> ExprBinaryOp $1 (OpIntMinus `oat` $2) $3 }
  | Expr '==' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntEq `oat` $2) $3 }
  | Expr '>=' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntGreaterEq `oat` $2) $3 }
  | Expr '<=' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpIntLessEq `oat` $2) $3 }
  | Expr '<-' Expr          { oc3 $1 $> ExprBinaryOp $1 (OpSet `oat` $2) $3 }
  | Expr '&' Expr           { oc2 $1 $> ExprOnion $1 $3 }
  | ApplExpr                { $1 }

ApplExpr :: { SPositional Expr }
  : PrefixExpr ApplExpr     { oc2 $1 $> ExprAppl $1 $2 }
  | PrefixExpr              { $1 }

PrefixExpr :: { SPositional Expr }
  : Label PrefixExpr        { oc2 $1 $> ExprLabelExp $1 $2 }
  | 'ref' PrefixExpr        { oc1 $1 $> ExprRef $2 }
  | PrimaryExpr             { $1 }

PrimaryExpr :: { SPositional Expr }
  : Var                     { oc1 $1 $> ExprVar $1 }
  | LiteralExpr             { $1 }
  | '(' Expr ')'            { $2 }

Var :: { SPositional Var }
  : ident                   { oc1 $1 $> Var $1 }
  
Label :: { SPositional LabelName }
  : label                   { oc1 $1 $> LabelName $1 }

LiteralExpr :: { SPositional Expr }
  : '()'                    { oc0 $1 $> ExprValEmptyOnion }
  | litint                  { oc1 $1 $> ExprValInt $1 }

Pattern :: { SPositional Pattern }
  : Pattern '&' Pattern     { oc2 $1 $> ConjunctionPattern $1 $3 }
  | PrimaryPattern          { $1 }

PrimaryPattern :: {SPositional Pattern }
  : Label PrimaryPattern    { oc2 $1 $> LabelPattern $1 $2 }
  | 'ref' Var               { oc1 $1 $> RefPattern (oc1S VariablePattern $2) }
  | PrimitiveType           { oc1 $1 $> PrimitivePattern $1 }
  | '()'                    { oc0 $1 $> EmptyPattern }
  | Var                     { oc1 $1 $> VariablePattern $1 }
  | '(' Pattern ')'         { $2 }

PrimitiveType :: { SPositional PrimitiveType }
  : 'int'                   { PrimInt `at` $1  }

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
    posData <$> parseTokens toks

type ParserM a = Either String a

parseError :: [Token] -> ParserM a
parseError tokens = Left $ display tokens

}