{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

{-|
  The master module for the TinyBang parser.  This module re-exports the
  appropriate contents of the submodules to other modules to form the parser's
  interface.
-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParseErr
) where

import Control.Applicative ((<$),(<*),(<$>),(<*>))
import Control.Monad.Reader
import Data.Monoid
import Text.Parsec

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Tokens
import Language.TinyBang.Utils.Parser.OriginParser
import Language.TinyBang.Utils.Parser.Parsec
import Language.TinyBang.Utils.Parser.PositionalParser

parseTinyBang :: SourceDocument -> [PositionalToken] -> Either ParseErr Expr
parseTinyBang doc toks =
  let x = runParserT pProgram mempty (nameOfDocument doc) toks in
  either (Left . show) Right $ runReader (unParserM x) (ParserContext doc)

type ParseErr = String

newtype ParserContext = ParserContext SourceDocument

newtype ParserM a
  = ParserM
      { unParserM :: Reader ParserContext a
      }
  deriving (Monad, MonadReader ParserContext)

instance PositionalParserMonad ParserM ParserContext where
  parserContext = ask
  parserSourceDocument = do
    ParserContext doc <- ask
    return doc
  runPositionalParser x = runReader (unParserM x)
  
-- |The type of the TinyBang parser.
type TBParser a =
  ParsecT [PositionalToken] (SimplePositionalState PositionalToken) ParserM a

-- * Non-leaf definitions

pProgram :: TBParser Expr
pProgram = pExpr <* eof

pExpr :: TBParser Expr
pExpr = "expression" <@>
  origConstr1 Expr $% sepEndBy1 pClause (consume TokSemi)

pClause :: TBParser Clause
pClause = "clause" <@>
      origConstr2 Clause $% (,) <$> pVar <* consume TokIs <*> pRedex ?+> eps

pRedex :: TBParser Redex
pRedex = "redex" <@>
      origConstr2 Appl $% (,) <$> pVar <*> pVar ?+> eps
  <|> origConstr2 Builtin $% (,) <$> pOp ?=> pVars
  <|> origConstr1 Def pValue
  <|> origConstr1 Copy pVar

pVar :: TBParser Var
pVar = "variable" <@> origConstr1 mkvar pIdent ?+> eps

pVars :: TBParser [Var]
pVars = "variable list" <@> many pVar

pValue :: TBParser Value
pValue = "value" <@>
      origConstr1 VPrimitive $% origConstr1 VInt $% pInt
  <|> requirex TokEmptyOnion VEmptyOnion
  <|> origConstr2 VLabel $% (,) <$> pLabel ?=> pVar
  <|> origConstr2 VOnion $% (,) <$> pVar <* consume TokOnion ?=> pVar
  <|> origConstr2 VScape $%
        (,) <$ consume TokStartBlock <*> pPattern <* consume TokStopBlock
            <* consume TokArrow
            <* consume TokStartBlock <*> pExpr <* consume TokStopBlock

pPattern :: TBParser Pattern
pPattern = "pattern" <@>
      origConstr1 Pattern $% sepEndBy1 pPatternClause (consume TokSemi)

pPatternClause :: TBParser PatternClause
pPatternClause = "pattern clause" <@>
  origConstr2 PatternClause $% (,) <$> pVar <* consume TokIs <*> pPatternValue

pPatternValue :: TBParser PatternValue
pPatternValue = "pattern value" <@>
      requirex TokInt $% flip PPrimitive PrimInt
  <|> requirex TokEmptyOnion PEmptyOnion
  <|> origConstr2 PLabel $% (,) <$> pLabel ?=> pVar
  <|> origConstr2 PConjunction $% (,) <$> pVar <* consume TokOnion ?=> pVar

-- * Terminal definitions

pIdent :: TBParser String
pIdent = require (\t ->
            case posToken t of
              TokIdentifier s -> Just s
              _ -> Nothing)
      <?> "identifier"

pInt :: TBParser Integer
pInt = require (\t ->
            case posToken t of
              TokLitInt n -> Just n
              _ -> Nothing)
      <?> "integer literal"

pLabel :: TBParser LabelName
pLabel = origConstr1 LabelName $% require (\t ->
            case posToken t of
              TokLabel n -> Just n
              _ -> Nothing)
      <?> "label name"

pOp :: TBParser BuiltinOp
pOp = require (\t ->
            case posToken t of
              TokPlus -> Just OpIntPlus
              TokMinus -> Just OpIntMinus
              TokEq -> Just OpIntEq
              TokLessEq -> Just OpIntLessEq
              TokGreaterEq -> Just OpIntGreaterEq
              _ -> Nothing)
      <?> "built-in operator"

-- * Utility definitions

-- |This parser is a specialization of @require@ which demands exact token
--  equality.  If it is matched, the second argument is applied to the origin
--  of the token and the result is returned.
requirex :: Token -> (Origin -> a) -> TBParser a
requirex t f =
  (
    f <$> fst <$> originParser
          (require $ \pt' -> if t == posToken pt' then Just () else Nothing)
  ) <?> display t

-- |Consumes a single token.  This is a specialization of @requirex@ which
--  always returns unit.
-- |This parser is a specialization of @requirex@ which produces a unit for all
--  matched input.
consume :: Token -> TBParser ()
consume t = display t <@> requirex t (const ())

