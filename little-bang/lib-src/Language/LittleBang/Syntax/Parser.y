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
import Language.LittleBang.Syntax.Tokens
import Language.LittleBang.Ast as LB
import qualified Language.TinyBangNested.Ast as TBN
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
  'fun'         { Token (SomeToken TokLambda $$) }
  'if'          { Token (SomeToken TokIf $$) }
  'then'        { Token (SomeToken TokThen $$) }
  'else'        { Token (SomeToken TokElse $$) }
  'object'      { Token (SomeToken TokObject $$) }
  'class'       { Token (SomeToken TokClass $$) }
  'getChar'     { Token (SomeToken TokGetChar $$) }
  'putChar'     { Token (SomeToken TokPutChar $$) }
  '->'          { Token (SomeToken TokArrow $$) }
  '()'          { Token (SomeToken TokEmptyOnion $$) }
  '=='          { Token (SomeToken TokEq $$) }
  '<='          { Token (SomeToken TokLessEq $$) }
  '>='          { Token (SomeToken TokGreaterEq $$) }
  '<'           { Token (SomeToken TokLess $$) }
  '<-'          { Token (SomeToken TokSet $$) }
  '&'           { Token (SomeToken TokOnion $$) }
  '='           { Token (SomeToken TokIs $$) }
  '('           { Token (SomeToken TokOpenParen $$) }
  ')'           { Token (SomeToken TokCloseParen $$) }
  '['           { Token (SomeToken TokOpenBracket $$) }
  ']'           { Token (SomeToken TokCloseBracket $$) }
  '{'           { Token (SomeToken TokOpenBrace $$) }
  '}'           { Token (SomeToken TokCloseBrace $$) }
  '+'           { Token (SomeToken TokPlus $$) }
  '-'           { Token (SomeToken TokMinus $$) }
  ':'           { Token (SomeToken TokColon $$) }
  ';'           { Token (SomeToken TokSemi $$) }
  ','           { Token (SomeToken TokComma $$) }
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }
  litchar       { Token (SomeToken TokLitChar $$) }
  '::'          { Token (SomeToken TokCons $$) }
  '!'           { Token (SomeToken TokDeref $$) }
  '.'           { Token (SomeToken TokDot $$) }
  '~'           { Token (SomeToken TokTilde $$) }
  '*'           { Token (SomeToken TokAsterisk $$) }

%left LAM
%right 'in'
%right ';'
%right 'else'
%nonassoc '<=' '>=' '=='
%right '<-'
%right '::'
%left '+' '-'
%left '['
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
  | Expr ';' Expr           { oc3 $1 $> LExprBinaryOp $1 (LB.OpSeq `oat` $2) $3 }
  | Expr '::' Expr          { oc3 $1 $> LExprBinaryOp $1 (LB.OpCons `oat` $2) $3 }
  | '[' ExprList ']'        { oc1 $1 $> LExprList $2 }
  | '{' ArgList '}'         { oc1 $1 $> LExprRecord $2 }
  | 'object' '{' ObjTermList '}'
                            { oc1 $1 $> LExprObject $3 }
  | 'class' '(' ParamList ')' maybe(SubclassTerm) '{' ClassTermList '}'
                            { oc3 $1 $> LExprClass $3 $7 (vpos $ posData `fmap` $5) }
  | 'class' '()' maybe(SubclassTerm) '{' ClassTermList '}'
                            { oc3 $1 $> LExprClass (vpos []) $5 (vpos $ posData `fmap` $3) }
  | InvokableExpr '(' ArgList ')'
                            { oc2 $1 $> LExprAppl $1 $3 }
  | InvokableExpr '()'
                            { oc2 $1 $> LExprAppl $1 (vpos []) }
  | PrefixExpr              { $1 }
  | Expr '[' Expr ']'       { oc2 $1 $> LExprIndexedList $1 $3 }
  | 'getChar'               { oc0 $1 $> TExprGetChar }
  | 'putChar' Expr          { oc1 $1 $> TExprPutChar $2 }

PrefixExpr :: { SPositional Expr }
  : Label PrefixExpr        { oc2 $1 $> TExprLabelExp $1 $2 }
  | 'ref' PrefixExpr        { oc1 $1 $> TExprRef $2 }
  | '!' PrefixExpr          { oc1 $1 $> LExprDeref $2 }
  | PrimaryExpr             { $1 }

PrimaryExpr :: { SPositional Expr }
  : LiteralExpr             { $1 }
  | InvokableExpr           { $1 }
  | PrimaryExpr '.' Ident   { oc2 $1 $> LExprProjection $1 $3 }
  | PrimaryExpr '.' Ident '(' ArgList ')'
                            { oc3 $1 $> LExprDispatch $1 $3 $5 }
  | PrimaryExpr '.' Ident '()'
                            { oc3 $1 $> LExprDispatch $1 $3 (vpos []) }

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
  | litchar                 { oc1 $1 $> TExprValChar $1 }
  
ParamList :: { VPositional [Param] }
  : manySepOpt( Param , ',' )
                            { $1 }

Param :: { SPositional Param }
  : Ident ':' Pattern       { oc2 $1 $> Param $1 $3 }
  | Ident                   { oc2 $1 $> Param $1 (oc0 $1 $> EmptyPattern) } 

Pattern :: { SPositional Pattern }
  : Pattern '*' Pattern     { oc2 $1 $> ConjunctionPattern $1 $3 }
  | Pattern '::' Pattern    { oc2 $1 $> ConsPattern $1 $3 }
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
  | 'char'                  { PrimChar `at` $1  }

PatternList :: { VPositional [Pattern] }
  : manySepOpt( Pattern , ',' )
                            { $1 }

ArgList :: { VPositional [Arg] }
  : manySepOpt( Arg , ',' ) { $1 }

Arg :: { SPositional Arg }
  : Expr                    { oc1 $1 $> PositionalArg $1 }
  | Ident '=' Expr          { oc2 $1 $> NamedArg $1 $3 }

ObjTermList :: { VPositional [ObjectTerm] }
  : many( ObjTerm )         { $1 }

ObjTerm :: { SPositional ObjectTerm }
  : Ident '(' ParamList ')' '=' Expr
                            { oc3 $1 $> ObjectMethod $1 $3 $6 }
  | Ident '()' '=' Expr
                            { oc3 $1 $> ObjectMethod $1 (vpos []) $4 }
  | Ident '=' Expr          { oc2 $1 $> ObjectField $1 $3 }

ClassTermList :: { VPositional [ClassTerm] }
  : many( ClassTerm )       { $1 }

ClassTerm :: { SPositional ClassTerm }
  : ObjTerm              { oc1 $1 $> ClassInstanceProperty $1 }
  | '~' ObjTerm           { oc1 $1 $> ClassStaticProperty $2 }

SubclassTerm :: { SPositional Ident }
  : '<' Ident            { $2 }

-- Generalizations of common grammar patterns.

maybe(p)
  : p                       { Just $1 }
  | {- epsilon -}           { Nothing }

many(p)
  : maybe(many1(p))         { maybe (vpos []) forgetSpan $1 }

many1(p)
  : p                       { fmap (:[]) $1 }
  | p many1(p)              { posOver $1 $> $ posData $1 : posData $2 }

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
