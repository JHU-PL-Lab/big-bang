{-# LANGUAGE LambdaCase #-}
{
module Test.TinyBang.TestUtils.ExpectDsl.Parser
( parseValueDsl
) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Utils.Display
import Test.TinyBang.TestUtils.ExpectDsl.Data
import Test.TinyBang.TestUtils.ExpectDsl.Tokens
}

%name parseTokens
%monad { ParserM }
%tokentype { Token }
%error { parseError }

%token
  '()'          { TokEmptyOnion }
  '&'           { TokOnion }
  '('           { TokOpenParen }
  ')'           { TokCloseParen }
  litint        { TokLitInt $$ }
  label         { TokLabel $$ }
  'ref'         { TokRef }
  'scapes'      { TokScapes }
  'typefail'    { TokTypeFail }
  EOF           { TokEOF }
  
%left '&'

%%

Program
  : Expectation EOF         { $1 }

Expectation
  : Predicate               { \s -> Pass ($1) s }
  | 'typefail'              { const $ TypeFailure }
  
Predicate
  : Predicate '&' Predicate { \onion -> $1 onion && $3 onion }
  | PrimaryPredicate        { $1 }

PrimaryPredicate
  : LabelPredicate          { $1 }
  | RefPredicate            { $1 }
  | PrimitivePredicate      { $1 }
  | 'scapes'                { \onion -> not $ null $ deepScapes onion }
  | '(' Predicate ')'       { $2 }
  
LabelPredicate
  : label PrimaryPredicate  { demandLabel $1 $2 }

RefPredicate
  : 'ref' PrimaryPredicate  { demandRef $2 }

PrimitivePredicate
  : litint                  { demandPrim PrimInt $ VInt generated $1 }
  | '()'                    { const True }

{

demandLabel :: String -> DeepOnionPredicate -> DeepOnionPredicate
demandLabel n inner onion =
  fromMaybe False $
    inner <$> Map.lookup (LabelName generated n) (deepLabels onion)

demandRef :: DeepOnionPredicate -> DeepOnionPredicate
demandRef inner onion =
  fromMaybe False $ inner <$> deepRef onion

demandPrim :: PrimitiveType -> PrimitiveValue -> DeepOnionPredicate
demandPrim dpt val onion = fromMaybe False $
  (==) val <$> Map.lookup dpt (deepPrimitives onion)

parseValueDsl :: [Token] -> Either String (String -> Expectation)
parseValueDsl toks = parseTokens toks

type ParserM a = Either String a

parseError :: [Token] -> ParserM a
parseError tokens = Left $ display tokens

}