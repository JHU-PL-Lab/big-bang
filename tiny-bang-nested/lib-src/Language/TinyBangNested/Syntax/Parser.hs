{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Language.TinyBangNested.Syntax.Parser
( parseTinyBangNested
, parseTinyBangNestedPattern
, ParseErr
) where

import Control.Applicative hiding ((<|>))
import Control.Monad.Reader
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Data.Monoid

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Parser.OriginParser
import Language.TinyBang.Utils.Parser.Parsec
import Language.TinyBang.Utils.Parser.PositionalParser
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Ast

-- |A function to parse TinyBangNested code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseTinyBangNested :: SourceDocument
                    -> [PositionalToken]
                    -> Either ParseErr Expr
parseTinyBangNested = parseTinyBangNestedRule pProgram
    
-- |A function to parse TinyBangNested code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseTinyBangNestedPattern :: SourceDocument
                           -> [PositionalToken]
                           -> Either ParseErr Pattern
parseTinyBangNestedPattern = parseTinyBangNestedRule pPattern

parseTinyBangNestedRule :: TBNParser a
                        -> SourceDocument
                        -> [PositionalToken]
                        -> Either ParseErr a
parseTinyBangNestedRule parser doc toks =
  let x = runParserT parser mempty (nameOfDocument doc) toks in
  either (Left . show) Right $ runReader (unParserM x) (ParserContext doc)
        
-- * Supporting data types

-- |Defines the type of error for the TBN parser.
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
  
-- |The type of the TinyBang Nested parser.
type TBNParser a =
  ParsecT [PositionalToken] (SimplePositionalState PositionalToken) ParserM a

-- * Parser definitions

-- |A program is a single expression.
pProgram :: TBNParser Expr
pProgram = pExpr <* eof

-- ** Expression parsers

-- |Expressions start at "let" priority.
pExpr :: TBNParser Expr
pExpr = pLetExpr

-- |"let" priority is either a @let@ expression or "function" priority.
pLetExpr :: TBNParser Expr
pLetExpr = "let expression" <@>
      origConstr3 ExprLet $%
        (,,) <$ consume TokLet
             ?=> pVar
             <* consume TokIs
             <*> pLetExpr
             <* consume TokIn
             <*> pLetExpr
  <|> pFuncExpr

-- |"function" priority is either a scape expression or "arithmetic" priority
pFuncExpr :: TBNParser Expr
pFuncExpr = "function expression" <@>
      origConstr2 ExprScape $%
        (,) <$> pPattern
            <* consume TokArrow
            ?=> pLetExpr
  <|> pBinaryArithExpr

-- |"arithmetic" priority is either a scape expression or "onion" priority
pBinaryArithExpr :: TBNParser Expr
pBinaryArithExpr = "binary arithmetic expression" <@>
      try (origLeftAssocBinOp ExprBinaryOp pOnionExpr pBinaryArithOp)
  <|> pOnionExpr

-- |"onion" priority is either an onion or "application" priority
pOnionExpr :: TBNParser Expr
pOnionExpr = "onion expression" <@>
      try (origLeftAssocBinOp onion pApplExpr $% consume TokOnion)
  <|> pApplExpr
  where
    onion o e1 () e2 = ExprOnion o e1 e2

-- |"application" priority is either an application or "label" priority
pApplExpr :: TBNParser Expr
pApplExpr = "application expression" <@>
      try (origLeftAssocBinOp appl pLabelExpr $% consume TokOnion)
  <|> pLabelExpr
  where
    appl o e1 () e2 = ExprAppl o e1 e2

-- |"label" priority is either a label construction or "primary" priority
pLabelExpr :: TBNParser Expr
pLabelExpr = "label expression" <@>
      origConstr2 ExprLabelExp $% (,) <$> pLabel ?=> pPrimaryExpr
  <|> pPrimaryExpr

-- |"primary" priority is a variable, a primitive literal, an empty onion, or
--  a parenthesized expression
pPrimaryExpr :: TBNParser Expr
pPrimaryExpr = "primary expression" <@>
      origConstr1 ExprVar pVar
  <|> pLiteral
  <|> try (consume TokOpenParen >> pExpr <* consume TokCloseParen)
  
-- |Parses literal value expressions.
pLiteral :: TBNParser Expr
pLiteral = "literal expression" <@>
      origConstr1 ExprValInt pInt
  <|> try $% ExprValEmptyOnion <$> (fst <$> originParser (consume TokEmptyOnion))

-- ** Pattern parsers

-- |Patterns start at "conjunction" priority
pPattern :: TBNParser Pattern
pPattern = pConjPattern

-- |"conjunction" priority is either a pattern conjunction or "label" priority
pConjPattern :: TBNParser Pattern
pConjPattern = "conjunction pattern" <@>
      origConstr2 ConjunctionPattern $%
        (,) <$> pLabelPattern <* consume TokOnion ?=> pConjPattern
  <|> pLabelPattern

-- |"label" priority is either a label pattern or "primary" priority
pLabelPattern :: TBNParser Pattern
pLabelPattern = "label pattern" <@>
      origConstr2 LabelPattern $% (,) <$> pLabel ?=> pLabelPattern
  <|> pPrimaryPattern

-- |"primary" priority is a primitive type, an empty onion, a variable, or a
--  parenthesized pattern
pPrimaryPattern :: TBNParser Pattern
pPrimaryPattern = "primary pattern" <@>
      origConstr1 PrimitivePattern pPrimitiveType
  <|> EmptyPattern <$> (fst <$> originParser (consume TokEmptyOnion))
  <|> origConstr1 VariablePattern pVar
  <|> try (consume TokOpenParen >> pPattern <* consume TokCloseParen)

-- ** Supporting non-terminal parsers

-- |Parses variables.
pVar :: TBNParser Var
pVar = "variable" <@> origConstr1 Var pIdent ?+> eps

-- * Terminal definitions

pIdent :: TBNParser String
pIdent = require (\t ->
            case posToken t of
              TokIdentifier s -> Just s
              _ -> Nothing)
      <?> "identifier"

pInt :: TBNParser Integer
pInt = require (\t ->
            case posToken t of
              TokLitInt n -> Just n
              _ -> Nothing)
      <?> "integer literal"

pLabel :: TBNParser LabelName
pLabel = origConstr1 LabelName $% require (\t ->
            case posToken t of
              TokLabel n -> Just n
              _ -> Nothing)
      <?> "label name"

pBinaryArithOp :: TBNParser BinaryOperator
pBinaryArithOp = (do
    (origin, constr) <- originParser $
              require
                (\t ->
                  case posToken t of
                    TokPlus -> Just OpIntPlus
                    TokMinus -> Just OpIntMinus
                    TokEq -> Just OpIntEq
                    TokLessEq -> Just OpIntLessEq
                    TokGreaterEq -> Just OpIntGreaterEq
                    _ -> Nothing)
    return $ constr origin
  ) <?> "built-in operator"

pPrimitiveType :: TBNParser PrimitiveType
pPrimitiveType = require (\t ->
                    case posToken t of
                      TokInt -> Just PrimInt
                      _ -> Nothing)
    <?> "primitive type"
  
-- * Utility definitions

-- |This parser is a specialization of @require@ which demands exact token
--  equality.  If it is matched, the second argument is applied to the origin
--  of the token and the result is returned.
requirex :: Token -> (Origin -> a) -> TBNParser a
requirex t f =
  (
    f <$> fst <$> originParser
          (require $ \pt' -> if t == posToken pt' then Just () else Nothing)
  ) <?> display t

-- |Consumes a single token.  This is a specialization of @requirex@ which
--  always returns unit.
-- |This parser is a specialization of @requirex@ which produces a unit for all
--  matched input.
consume :: Token -> TBNParser ()
consume t = display t <@> requirex t (const ())

