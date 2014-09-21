{
{-# LANGUAGE GADTs #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

import Language.TinyBang.Ast
import Language.TinyBang.Syntax.Tokens
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax 
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
  'getChar'     { Token (SomeToken TokGetChar $$) }
  'putChar'     { Token (SomeToken TokPutChar $$) }
  '->'          { Token (SomeToken TokArrow $$) }
  '()'          { Token (SomeToken TokEmptyOnion $$) }
  '=='          { Token (SomeToken TokEq $$) }
  '<='          { Token (SomeToken TokLessEq $$) }
  '>='          { Token (SomeToken TokGreaterEq $$) }
  '<-'          { Token (SomeToken TokSet $$) }
  '&'           { Token (SomeToken TokOnion $$) }
  '='           { Token (SomeToken TokIs $$) }
  ';'           { Token (SomeToken TokSemi $$) }
  '{'           { Token (SomeToken TokStartBlock $$) }
  '}'           { Token (SomeToken TokStopBlock $$) }
  '+'           { Token (SomeToken TokPlus $$) }
  '-'           { Token (SomeToken TokMinus $$) }
  '*'           { Token (SomeToken TokMult $$) }
  '/'           { Token (SomeToken TokDiv $$) }
  '%'           { Token (SomeToken TokMod $$) }
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }
  litchar       { Token (SomeToken TokLitChar $$) }

%%

Program
  : Expr eof                { $1 }

Expr
  : Clauses                 { oc1 $1 $> Expr $1 }

Clauses
  : many1SepOpt(Clause,';') { $1 }

Clause
  : Var '=' Redex           { oc2 $1 $> Clause $1 $3 }

Redex
  : Var Var                 { oc2 $1 $> Appl $1 $2 }
  | BuiltinOp Vars          { oc2 $1 $> Builtin $1 $2 }
  | Var                     { oc1 $1 $> Copy $1 }
  | Value                   { oc1 $1 $> Def $1 }
  | 'getChar'               { oc0 $1 $> GetChar }
  | 'putChar' Var           { oc1 $1 $> PutChar $2 }
 
Var
  : Ident                   { oc1 $1 $> mkvar $1 }

Vars
  : many1(Var)               { $1 }

Value
  : LiteralPrimitive        { oc1 $1 $> VPrimitive $1 }
  | '()'                    { oc0 $1 $> VEmptyOnion }
  | Label Var               { oc2 $1 $> VLabel $1 $2 }
  | 'ref' Var               { oc1 $1 $> VRef $2 }
  | Var '&' Var             { oc2 $1 $> VOnion $1 $3 }
  | '{' Pattern '}' '->' '{' Expr '}'
                            { oc2 $1 $> VScape $2 $6 }
Pattern
  : PatternClauses          { oc1 $1 $> Pattern $1 }

PatternClauses
  : many1SepOpt(PatternClause, ';')
                            { $1 }
  
PatternClause
  : Var '=' PatternValue    { oc2 $1 $> PatternClause $1 $3 }

PatternValue
  : 'int'                   { oc0 $1 $> (\o -> PPrimitive o PrimInt) }
  | 'char'                  { oc0 $1 $> (\o -> PPrimitive o PrimChar) }
  | '()'                    { oc0 $1 $> PEmptyOnion }
  | Label Var               { oc2 $1 $> PLabel $1 $2 }
  | 'ref' Var               { oc1 $1 $> PRef $2 }
  | Var '&' Var             { oc2 $1 $> PConjunction $1 $3 }

Ident
  : ident                   { posOver $1 $> $ posData $1 }

Label
  : label                   { oc1 $1 $> LabelName $1 }

LiteralPrimitive
  : litint                  { oc1 $1 $> VInt $1 }
  | litchar                 { oc1 $1 $> VChar $1 }

BuiltinOp
  : '+'                     { posOver $1 $> OpIntPlus }
  | '-'                     { posOver $1 $> OpIntMinus }
  | '*'                     { posOver $1 $> OpIntMult }
  | '/'                     { posOver $1 $> OpIntDiv }
  | '%'                     { posOver $1 $> OpIntMod }
  | '=='                    { posOver $1 $> OpIntEq }
  | '<='                    { posOver $1 $> OpIntLessEq }
  | '>='                    { posOver $1 $> OpIntGreaterEq }
  | '<-'                    { posOver $1 $> OpSet }

-- Generalizations of common grammar patterns.

many1SepOpt(p,s)
  : p                       { fmap (:[]) $1 }
  | p s                     { posOver $1 $2 $ [posData $1] }
  | p s many1SepOpt(p,s)    { posOver $1 $3 $ posData $1 : posData $3 }

many1(p)
  : p                       { fmap (:[]) $1 }
  | p many1(p)              { posOver $1 $2 $ posData $1 : posData $2 } 

many(p)
  : many1(p)                { forgetSpan $1 }
  |                         { vpos [] }

{

parseTinyBang :: SourceDocument -> [Token] -> Either String Expr
parseTinyBang _ toks =
    posData <$> parseTokens toks

}
