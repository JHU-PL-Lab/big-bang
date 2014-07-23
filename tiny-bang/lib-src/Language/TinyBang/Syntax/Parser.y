{
{-# LANGUAGE UndecidableInstances #-}

module Language.TinyBang.Syntax.Parser
(
) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Tokens 
}

%name parseTokens
%monad { ParserM }
%tokentype { PositionalToken }
%error { parseError }

%token
  'int'         { PositionalToken _ TokInt } 
  'ref'         { PositionalToken _ TokRef }
  '->'          { PositionalToken _ TokArrow }
  '()'          { PositionalToken _ TokEmptyOnion }
  '=='          { PositionalToken _ TokEq }
  '<='          { PositionalToken _ TokLessEq }
  '>='          { PositionalToken _ TokGreaterEq }
  '<-'          { PositionalToken _ TokSet }
  '&'           { PositionalToken _ TokOnion }
  '='           { PositionalToken _ TokIs }
  '('           { PositionalToken _ TokOpenParen }
  ')'           { PositionalToken _ TokCloseParen }
  ';'           { PositionalToken _ TokSemi }
  '{'           { PositionalToken _ TokStartBlock }
  '}'           { PositionalToken _ TokStopBlock }
  '+'           { PositionalToken _ TokPlus }
  '-'           { PositionalToken _ TokMinus }
  ident         { PositionalToken _ (TokIdentifier _) }
  label         { PositionalToken _ (TokLabel _) }
  litint        { PositionalToken _ (TokLitInt _) }

%%

Program
  : Expr                    { $1 }

Expr
  : Clauses                 { $1 }

Clauses
  : many(Clause)            { $1 }

Clause
  : Var '=' Redex ';'       { origOver $1 $> $ \o -> posApX2 Clause o $1 $3 }

Redex
  : Var Var                 { oc2 Appl $1 $2 }
  | BuiltinOp Vars          { oc2 Builtin $1 $2 }
  | Var                     { oc1 Copy $1 }
  | Value                   { oc1 Def $1 }
  
Var
  : Ident                   { oc1 mkvar $1 }

Vars
  : many(Var)               { $1 }

Value
  : LiteralPrimitive        { oc1 VPrimitive $1 }
  | '()'                    { posOver $1 $> VEmptyOnion }
  | Label Var               { oc2 VLabel $1 $2 }
  | 'ref' Var               { origOver $1 $> $ flip VRef $2 }
  | Var '&' Var             { oc2 VOnion $1 $3 }
  | '{' Pattern '}' '->' '{' Expr '}'
                            { origOver $1 $> $ \o -> VScape o $2 $6 }

Pattern
  : PatternClauses          { oc1 Pattern $1 }

PatternClauses
  : many1(PatternClause)    { $1 }
  
PatternClause
  : Var '=' PatternValue    { oc2 PatternClause $1 $3 }

PatternValue
  : 'int'                   { origOver $1 $> $ flip PPrimitive PrimInt }
  | '()'                    { origOver $1 $> PEmptyOnion }
  | Label Var               { oc2 PLabel $1 $2 }
  | 'ref' Var               { origOver $1 $> $ flip PRef $2 }
  | Var '&' Var             { oc2 PConjunction $1 $3 }

Ident
  : ident                   { posOver $1 $> $1 }

Label
  : label                   { posOver $1 $> $ LabelName $ ?????? $1 }

LiteralPrimitive
  : litint                  { posOver $1 $> $1 }

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
  : many1(p)                { $1 }
  |                         { vpos [] }
         

{

type ParserM a = Either String a

parseError :: [Token] -> ParserM a
parseError tokens = Left $ "Parse error: " + display tokens

-- |The type for "positional" data.  Positional data may or may not have a
--  @SourceSpan@ associated with it.  This association is used to provide spans
--  for the parsed AST nodes.  The expected values for @f@ are either @Identity@
--  or @Void@.
newtype Positional f a = Positional (f SourceSpan, a)

instance Functor (Positional f) where
  fmap f (Positional (s,d)) = Positional (s, f d)

instance HasSourceSpan (Positional Identity a) where
  spanOf = posSpan

-- |Used to discard type arguments.
newtype Void a = Void ()

-- |A type alias for spanned positional data.
type SPositional = Positional Identity 

-- |A type alias for unspanned positional data.
type VPositional = Positional Void

-- |Retrieves the payload from a positional value.
posData :: Positional f a -> a
posData (Positional (_, d)) = d

-- |Retrieves the span from a spanned positional value.
posSpan :: Positional Identity a -> SourceSpan
posSpan (Positional (Identity s, _)) = s

-- |A smart constructor for spanned positional data.
spos :: SourceSpan -> a -> SPositional a
spos s x = Positional (Identity s, x)

-- |A smart constructor for unspanned positional data.
vpos :: a -> VPositional a
vpos x = Positional (Void, x)

-- |A convenience mechanism for creating a positional over two things which have
--  source spans.
posOver :: (HasSourceSpan s1, HasSourceSpan s2)
        => s1 -> s2 -> a -> SPositional a
posOver s1 s2 = spos (spanOf s1 <--> spanOf s2)

-- |A convenience mechanism for creating a positional over two source-spanned
--  entities and a constructor which expects an origin.
origOver :: (HasSourceSpan s1, HasSourceSpan s2)
         => s1 -> s2 -> (Origin -> a) -> SPositional a
origOver s1 s2 f =
  let s = s1 <--> s2 in
  spos s $ f (SourceOrigin s)

-- |Origin-driven value constructor for one argument.  See @oc2@.
oc1 :: (Origin -> a1 -> r)
    -> SPositional a1
    -> SPositional r
oc1 f a1 =
  let s = posSpan a1 in
  spos s $ f (SourceOrigin s) (posData a1)

-- |Origin-driven value constructor for two extra arguments.  This function
--  accepts a constructor which expects an origin and two additional arguments.
--  Given those arguments in positional form, it generates an origin spanning
--  their positions and then passes their data as the remaining arguments.
oc2 :: (Origin -> a1 -> a2 -> r)
    -> SPositional a1
    -> SPositional a2
    -> SPositional r
oc2 f a1 a2 =
  let s = posSpan a1 <--> posSpan a2 in
  spos s $ f (SourceOrigin s) (posData a1) (posData a2)
  
-- |A function to extract data from two positional arguments during application.
--  Specifically, this function extracts from the /second/ and /third/ arguments
--  under the assumption that the first is not positional (e.g. an origin).
posApX2 :: (a0 -> a1 -> a2 -> r)
        -> a0
        -> Positional f1 a1
        -> Positional f2 a2
        -> r
posApX2 f a0 a1 a2 = f a0 (posData a1) (posData a2)

}