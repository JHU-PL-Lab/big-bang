{
{-# LANGUAGE GADTs #-}
module Language.LittleBang.Syntax.Parser
( parseLittleBang
, parseLittleBangPattern
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Maybe

import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax 
import Language.LittleBang.Ast
import Language.LittleBang.Syntax.Tokens
import qualified Language.TinyBangNested.Ast as TBN
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
  'if'          { Token (SomeToken TokIf $$) }
  'then'        { Token (SomeToken TokThen $$) }
  'else'        { Token (SomeToken TokElse $$) }
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
  '['           { Token (SomeToken TokOpenBracket $$) }
  ']'           { Token (SomeToken TokCloseBracket $$) }
  '+'           { Token (SomeToken TokPlus $$) }
  '-'           { Token (SomeToken TokMinus $$) }
  ':'           { Token (SomeToken TokColon $$) }
  ';'           { Token (SomeToken TokSemi $$) }
  ','           { Token (SomeToken TokComma $$) }
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }
  '::'          { Token (SomeToken TokCons $$) }
  '!'           { Token (SomeToken TokDeref $$) }

%left LAM
%right 'in'
%right ';'
%right 'else'
%nonassoc '<=' '>=' '=='
%right '<-'
%right '::'
%left '+' '-'
%left '&'

%name parseProgram Program
%name parsePattern Pattern

%%

Program :: { SPositional Expr }
  : Expr                    { $1 }

Expr :: { SPositional Expr }
  : 'let' Ident '=' Expr 'in' Expr
                            { oc3 $1 $> TExprLet $2 $4 $6 }
  | 'fun' ParamList '->' Expr %prec LAM
                            { oc2 $1 $> LExprScape $2 $4 }
  | 'if' Expr 'then' Expr 'else' Expr
                            { oc3 $1 $> LExprCondition $2 $4 $6 }
  | Expr '+' Expr           { oc3 $1 $> TExprBinaryOp $1 (TBN.OpIntPlus `oat` $2) $3 }
  | Expr '-' Expr           { oc3 $1 $> TExprBinaryOp $1 (TBN.OpIntMinus `oat` $2) $3 }
  | Expr '==' Expr          { oc3 $1 $> TExprBinaryOp $1 (TBN.OpIntEq `oat` $2) $3 }
  | Expr '>=' Expr          { oc3 $1 $> TExprBinaryOp $1 (TBN.OpIntGreaterEq `oat` $2) $3 }
  | Expr '<=' Expr          { oc3 $1 $> TExprBinaryOp $1 (TBN.OpIntLessEq `oat` $2) $3 }
  | Expr '<-' Expr          { oc3 $1 $> TExprBinaryOp $1 (TBN.OpSet `oat` $2) $3 }
  | Expr '&' Expr           { oc2 $1 $> TExprOnion $1 $3 }
  | Expr ';' Expr           { oc2 $1 $> LExprSequence $1 $3 }
  | Expr '::' Expr          { oc2 $1 $> LExprCons $1 $3 }
  | '[' ExprList ']'        { oc1 $1 $> LExprList $2 }
  | InvokableExpr '(' ArgList ')'
                            { oc2 $1 $> LExprAppl $1 $3 }
  | PrefixExpr              { $1 }

PrefixExpr :: { SPositional Expr }
  : Label PrefixExpr        { oc2 $1 $> TExprLabelExp $1 $2 }
  | 'ref' PrefixExpr        { oc1 $1 $> TExprRef $2 }
  | '!' PrefixExpr          { oc1 $1 $> LExprDeref $2 }
  | PrimaryExpr             { $1 }

PrimaryExpr :: { SPositional Expr }
  : LiteralExpr             { $1 }
  | InvokableExpr           { $1 }

InvokableExpr :: { SPositional Expr }
  : Ident                   { oc1 $1 $> TExprVar $1 }
  | '(' Expr ')'            { $2 }

ExprList :: { VPositional [Expr] }
  : manySepOpt( Expr , ',' )
                            { $1 }

Ident :: { SPositional Ident }
  : ident                   { oc1 $1 $> Ident $1 }
  
Label :: { SPositional LabelName }
  : label                   { oc1 $1 $> LabelName $1 }

LiteralExpr :: { SPositional Expr }
  : '()'                    { oc0 $1 $> TExprValEmptyOnion }
  | litint                  { oc1 $1 $> TExprValInt $1 }
  
ParamList :: { VPositional [Param] }
  : manySepOpt( Param , ',' )
                            { $1 }

Param :: { SPositional Param }
  : Ident ':' Pattern       { oc2 $1 $> Param $1 $3 }
  | Ident                   { oc2 $1 $> Param $1 (oc0 $1 $> EmptyPattern) } 

Pattern :: { SPositional Pattern }
  : Pattern '&' Pattern     { oc2 $1 $> ConjunctionPattern $1 $3 }
  | '[' PatternList ']'     { fmap ($ Nothing) (oc1 $1 $> ListPattern $2) }
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

PatternList :: { VPositional [Pattern] }
  : manySepOpt( Pattern , ',' )
                            { $1 }

ArgList :: { VPositional [Arg] }
  : manySepOpt( Arg , ',' ) { $1 }

Arg :: { SPositional Arg }
  : Expr                    { oc1 $1 $> PositionalArg $1 }
  | Ident '=' Expr          { oc2 $1 $> NamedArg $1 $3 }

-- Generalizations of common grammar patterns.

maybe(p)
  : p                       { Just $1 }
  | {- epsilon -}           { Nothing }

manySepOpt(p,s)
  : maybe(many1SepOpt(p,s)) { maybe (vpos []) forgetSpan $1 }        

many1SepOpt(p,s)
  : p                       { fmap (:[]) $1 }
  | p s                     { fmap (:[]) $ posOver $1 $2 $ posData $1 }
  | p s many1SepOpt(p,s)    { posOver $1 $> $ posData $1 : posData $3 }

{

parseLittleBang :: SourceDocument -> [Token] -> Either String Expr
parseLittleBang _ toks =
    posData <$> parseProgram toks

parseLittleBangPattern :: SourceDocument -> [Token] -> Either String Pattern
parseLittleBangPattern _ toks =
    posData <$> parsePattern toks

}
