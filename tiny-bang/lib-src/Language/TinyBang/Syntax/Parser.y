{
{-# LANGUAGE GADTs #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Positional 
import Language.TinyBang.Syntax.Tokens 
}

%name parseTokens
%monad { ParserM }
%tokentype { Token }
%error { parseError }

%token
  'int'         { Token (SomeToken TokInt $$) } 
  'ref'         { Token (SomeToken TokRef $$) }
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
  ident         { Token (SomeToken TokIdentifier $$) }
  label         { Token (SomeToken TokLabel $$) }
  litint        { Token (SomeToken TokLitInt $$) }

%%

Program
  : Expr                    { $1 }

Expr
  : Clauses                 { oc1 $1 $> Expr $1 }

Clauses
  : many1(Clause)           { $1 }

Clause
  : Var '=' Redex ';'       { oc2 $1 $> Clause $1 $3 }

Redex
  : Var Var                 { oc2 $1 $> Appl $1 $2 }
  | BuiltinOp Vars          { oc2 $1 $> Builtin $1 $2 }
  | Var                     { oc1 $1 $> Copy $1 }
  | Value                   { oc1 $1 $> Def $1 }
  
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
  : many1(PatternClause)    { $1 }
  
PatternClause
  : Var '=' PatternValue    { oc2 $1 $> PatternClause $1 $3 }

PatternValue
  : 'int'                   { oc0 $1 $> (\o -> PPrimitive o PrimInt) }
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

BuiltinOp
  : '+'                     { posOver $1 $> OpIntPlus }
  | '-'                     { posOver $1 $> OpIntMinus }
  | '=='                    { posOver $1 $> OpIntEq }
  | '<='                    { posOver $1 $> OpIntLessEq }
  | '>='                    { posOver $1 $> OpIntGreaterEq }
  | '<-'                    { posOver $1 $> OpSet }

-- Generalizations of common grammar patterns.

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

type ParserM a = Either String a

parseError :: [Token] -> ParserM a
parseError tokens = Left $ "Parse error: " ++ display tokens

-- |A version of @oc2@ for constructors with /no/ additional arguments rather
--  than two.
oc0 :: SPositional s1
    -> SPositional s2
    -> (Origin -> r)
    -> SPositional r
oc0 s1 s2 f =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss)

-- |A version of @oc2@ for constructors with /one/ additional argument rather
--  than two.
oc1 :: SPositional s1
    -> SPositional s2
    -> (Origin -> a1 -> r)
    -> Positional f1 a1
    -> SPositional r
oc1 s1 s2 f a1 =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss) (posData a1)

-- |A tool for calling constructors which expect origins within this parser.
--  This function accepts the first and last token of the parse, a constructor
--  expecting an @Origin@ argument followed by two other arguments, and the
--  @Positional@ form of those two arguments.  This function then creates an
--  @Origin@ based on the provided tokens and calls the constructor with this
--  and the data parts of the remaining arguments.  The result is the positional
--  form of the result of the constructor.
oc2 :: SPositional s1
    -> SPositional s2
    -> (Origin -> a1 -> a2 -> r)
    -> Positional f1 a1
    -> Positional f2 a2
    -> SPositional r
oc2 s1 s2 f a1 a2 =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss) (posData a1) (posData a2)

}