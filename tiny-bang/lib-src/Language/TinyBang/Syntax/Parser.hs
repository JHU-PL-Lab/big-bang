{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns,
             TemplateHaskell #-}

module Language.TinyBang.Syntax.Parser
( ParserContext (..) 
, parseTinyBang
) where

import Control.Applicative           ((*>), (<$>), (<*), (<*>))
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Reader
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Language.TinyBang.Ast
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Parsec
import Utils.ParserUtils

$(loggingFunctions)

-- |A function to parse TinyBang code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseTinyBang :: ParserContext -> [PositionalToken] -> Either String Expr
parseTinyBang context ts =
  _debugI ("Parsing TinyBang source with tokens: " ++
            (display $ map posToken ts)) $
  let parseResult =
        runParserT programParser () (contextDocumentName context) ts
  in
  case runReader parseResult context of
    Left err -> Left $ show err
    Right expr -> Right expr
-- |The context in which parsing occurs.
data ParserContext = ParserContext
                      { contextDocument :: SourceDocument
                      , contextDocumentName :: String }

-- |A type alias for the monad in which parsing occurs.
type ParserMonad = Reader ParserContext

-- |A type alias for the TinyBang parser.
type Parser a = ParsecT [PositionalToken] () ParserMonad a

programParser :: Parser Expr
programParser = expressionParser <* eof

expressionParser :: Parser Expr
expressionParser = "Expression" <@> do
  cls <- sepBy1 clauseParser $ consume TokSemi
  return $ Expr (SourceOrigin $ coverRegion (head cls) (last cls)) cls

clauseParser :: Parser Clause
clauseParser = "Clause" <@>
      Evaluated <$> evaluatedClauseParser
  <|> argorig2 CellSet <$> cellVarParser <* consume TokGets ?=> flowVarParser
  <|> argorig2 CellGet <$> flowVarParser <*
        consume TokIs <* consume TokBang ?=> cellVarParser
  <|> argorig2 Throws <$> flowVarParser <* consume TokThrows ?=> flowVarParser
  <|> argorig2 RedexDef <$> flowVarParser <* consume TokIs ?=> redexParser

evaluatedClauseParser :: Parser EvaluatedClause
evaluatedClauseParser = "Evaluated Clause" <@>
      argorig2 ValueDef <$> flowVarParser <* consume TokIs <*> valueParser
        ?+> eps
  <|> argorig3 CellDef <$> cellQualifierParser <*> cellVarParser <*
        consume TokDef ?=> flowVarParser
  <|> argorig3 Flow <$> flowVarParser <*> flowKindParser ?=> flowVarParser

redexParser :: Parser Redex
redexParser = "Redex" <@>
      argorig2 Appl <$> flowVarParser <*> flowVarParser ?+> eps
  <|> argorig3 BinOp <$> flowVarParser <*> binaryOperatorParser
                          ?=> flowVarParser
  <|> argorig1 Define <$> flowVarParser ?+> eps

valueParser :: Parser Value
valueParser = "Value" <@>
      argorig2 VScape <$> patternParser <* consume TokArrow ?+>
        consume TokOpenBrace <*> expressionParser <* consume TokCloseBrace
  <|> VInt <$&> require matchIntLit
  <|> VChar <$&> require matchCharLit
  <|> requirex TokEmptyOnion VEmptyOnion
  <|> argorig2 VLabel <$> labelNameParser ?=> cellVarParser
  <|> argorig2 VOnion <$> flowVarParser <* consume TokOnion ?=> flowVarParser
  <|> argorig3 VOnionFilter <$> flowVarParser <*> onionOpParser ?=> projectorParser
        
flowKindParser :: Parser FlowKind
flowKindParser = snd <$> require matchFlows
  where
    matchFlows :: Token -> Maybe FlowKind
    matchFlows tok = case tok of
      TokFlows c -> case c of
        'x' -> Just FlowExn
        _ ->  Nothing
      _ -> Nothing

patternParser :: Parser Pattern
patternParser = "Pattern" <@>
      argorig2 ValuePattern <$> cellVarParser <*
        consume TokColon <*> innerPatternParser ?+> eps
  <|> argorig2 ValuePattern <$> (consume TokExn *> cellVarParser) ?+>
        consume TokColon <*> innerPatternParser

innerPatternParser :: Parser InnerPattern
innerPatternParser = "Inner Pattern" <@>
      argorig2 ConjunctionPattern <$> basicInnerPatternParser <*
        consume TokOnion ?=> innerPatternParser
  <|> basicInnerPatternParser
  
-- |A parser for non-onion patterns
basicInnerPatternParser :: Parser InnerPattern
basicInnerPatternParser = "Basic Inner Pattern" <@>
      consume TokOpenParen *> innerPatternParser ?+> consume TokCloseParen
  <|> argorig1 PrimitivePattern <$> primitiveTypeParser ?+> eps
  <|> argorig3 LabelPattern <$> labelNameParser ?=> cellVarParser <*
        consume TokColon <*> basicInnerPatternParser
  <|> requirex TokFun ScapePattern
  <|> requirex TokEmptyOnion EmptyOnionPattern

onionOpParser :: Parser OnionOp
onionOpParser =
      requirex TokOnionSub OpOnionSub
  <|> requirex TokOnionProj OpOnionProj

binaryOperatorParser :: Parser BinaryOperator
binaryOperatorParser =
      requirex TokPlus OpPlus
  <|> requirex TokMinus OpMinus
  <|> requirex TokMult OpMult
  <|> requirex TokEq OpEqual
  <|> requirex TokLT OpLess
  <|> requirex TokGT OpGreater

cellQualifierParser :: Parser CellQualifier
cellQualifierParser =
      requirex TokFinal QualFinal
  <|> requirex TokImmut QualImmutable
  <|> QualNone . SourceOrigin <$> (SourceRegion <$> parserLocation <*> parserLocation)

projectorParser :: Parser AnyProjector
projectorParser =
      SomeProjector <$> argorig1 ProjPrim <$> primitiveTypeParser
  <|> SomeProjector <$> argorig1 ProjLabel <$> labelNameParser
  <|> SomeProjector <$> requirex TokFun ProjFun

primitiveTypeParser :: Parser PrimitiveType
primitiveTypeParser =
      requirex TokInt PrimInt
  <|> requirex TokChar PrimChar

labelNameParser :: Parser LabelName
labelNameParser = LabelName <$&> require matchLabel

flowVarParser :: Parser FlowVar
flowVarParser = FlowVar <$&> require matchIdent

cellVarParser :: Parser CellVar
cellVarParser = CellVar <$&> require matchIdent

-- |A matching function for integer literals.
matchIntLit :: Token -> Maybe Integer
matchIntLit x =
  case x of
    TokLitInt n -> Just n
    _ -> Nothing

-- |A matching function for character literals.
matchCharLit :: Token -> Maybe Char
matchCharLit x =
  case x of
    TokLitChar c -> Just c
    _ -> Nothing

-- |A matching function which produces a string for identifier tokens and
--  nothing for all other tokens.
matchIdent :: Token -> Maybe String
matchIdent x =
  case x of
    TokIdentifier s -> Just s
    _ -> Nothing

-- |A matching function which produces a string for label tokens and
--  nothing for all other tokens.
matchLabel :: Token -> Maybe String
matchLabel x =
  case x of
    TokLabel s -> Just s
    _ -> Nothing

-- |This parser is a specialization of @requirex@ which produces a unit for all
--  matched input.
consume :: Token -> Parser ()
consume t = display t <@> requirex t (const ())

-- |This parser is a specialization of the @token@ parser with the necessary
--  pretty-printing and location-calculating routines embedded.
require :: forall a. (Token -> Maybe a) -> Parser (Origin, a)
require f = do
    context <- parserContext
    document <- parserDocument
    tokenPrim show (nextPos context) (matchToken document)
    where
     nextPos :: ParserContext
                -> SourcePos
                -> PositionalToken
                -> [PositionalToken]
                -> SourcePos
     nextPos context _ tok toks =
        case runReader (uncons toks) context of
                  Nothing -> startPos tok
                  Just (tok',_) -> startPos tok'
     textPos :: SourceDocument -> SourcePos -> SourceLocation
     textPos document p = TextSource document (sourceLine p) (sourceColumn p)
     matchToken :: SourceDocument -> PositionalToken -> Maybe (Origin, a)
     matchToken doc ptok = do -- Maybe
      result <- f $ posToken ptok
      let reg = SourceRegion (textPos doc $ startPos ptok)
                             (textPos doc $ stopPos ptok)
      return (SourceOrigin reg, result)

-- |This parser is a specialization of @require@ which demands exact token
--  equality.  If it is matched, the second argument is applied to the origin
--  of the token and the result is returned.
requirex :: Token -> (Origin -> a) -> Parser a
requirex t f = do
  (orig,_) <- require $ \t' -> if t == t' then Just () else Nothing
  return $ f orig

-- |Determines the current parser location.
parserLocation :: Parser SourceLocation
parserLocation = do
  doc <- parserDocument
  pos <- getPosition
  return $ TextSource doc (sourceLine pos) (sourceColumn pos)

-- |A mechanism to retrieve the parser context.
parserContext :: Parser ParserContext
parserContext = lift ask

-- |A simple mechanism for retrieving the resource name for the current parse
--  context.
parserDocument :: Parser SourceDocument  
parserDocument = lift (contextDocument <$> ask)

