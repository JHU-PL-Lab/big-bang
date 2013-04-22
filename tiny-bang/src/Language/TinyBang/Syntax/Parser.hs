{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParserContext(..)
) where

import Control.Applicative           ((*>), (<$>), (<*), (<*>))
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Reader
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec hiding (Parser)

import Language.TinyBang.Ast
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location

-- |A function to parse TinyBang code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseTinyBang :: ParserContext -> [PositionalToken] -> Either String Expr
parseTinyBang context ts =
  let parseResult =
        runParserT expressionParser () (contextDocumentName context) ts
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

expressionParser :: Parser Expr
expressionParser = do
  cls <- sepBy1 clauseParser $ consume TokSemi
  return $ Expr (coverRegion (head cls) (last cls)) cls

clauseParser :: Parser Clause
clauseParser =
      Evaluated <$> evaluatedClauseParser
  <|> argreg2 RedexDef <$> flowVarParser <*> redexParser
  <|> argreg2 CellSet <$> cellVarParser <*> (consume TokGets *> flowVarParser)
  <|> argreg2 CellGet <$> flowVarParser <*> (consume TokGets *> cellVarParser)
  <|> argreg2 Throws <$> flowVarParser <*> (consume TokThrows *> flowVarParser)

evaluatedClauseParser :: Parser EvaluatedClause
evaluatedClauseParser =
      argreg2 ValueDef <$> flowVarParser <*> (consume TokIs *> valueParser)
  <|> argreg3 CellDef <$> cellQualifierParser <*> cellVarParser <*>
        (consume TokIs  *> flowVarParser)
  <|> argreg3 Flow <$> flowVarParser <*> flowKindParser <*> flowVarParser

redexParser :: Parser Redex
redexParser =
      argreg2 Appl <$> flowVarParser <*> flowVarParser
  <|> argreg3 BinOp <$> flowVarParser <*> binaryOperatorParser <*> flowVarParser

valueParser :: Parser Value
valueParser =
      VInt <$&> require matchIntLit
  <|> VChar <$&> require matchCharLit
  <|> requirex TokEmptyOnion VEmptyOnion
  <|> argreg2 VLabel <$> labelNameParser <*> cellVarParser
  <|> argreg2 VOnion <$> flowVarParser <*> (consume TokOnion *> flowVarParser)
  <|> argreg3 VOnionFilter <$> flowVarParser <*> onionOpParser <*> projectorParser
  <|> argreg2 VScape <$> patternParser <*>
        (consume TokOpenBrace *> expressionParser <* consume TokCloseBrace)
        
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
patternParser =
      argreg2 ValuePattern <$> cellVarParser <*>
        (consume TokColon *> innerPatternParser)
  <|> argreg2 ValuePattern <$> (consume TokExn *> cellVarParser) <*>
        (consume TokColon *> innerPatternParser)

innerPatternParser :: Parser InnerPattern
innerPatternParser =
      consume TokOpenParen *> innerPatternParser <* consume TokCloseParen
  <|> argreg1 PrimitivePattern <$> primitiveTypeParser
  -- note that this ordering makes conjunction bind more loosely than labeling;
  -- e.g. `A x:int & `B y:int is (`A x:int) & (`B y:int) and not
  -- `A x:(int & `B y:int)
  <|> argreg2 ConjunctionPattern <$> innerPatternParser <*>
        (consume TokOnion *> innerPatternParser)
  <|> argreg3 LabelPattern <$> labelNameParser <*> cellVarParser <*>
        (consume TokColon *> innerPatternParser)
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
  <|> requirex TokEq OpEqual
  <|> requirex TokLT OpLess
  <|> requirex TokGT OpGreater

cellQualifierParser :: Parser CellQualifier
cellQualifierParser =
      requirex TokFinal QualFinal
  <|> requirex TokImmut QualImmutable
  <|> QualNone <$> (SourceRegion <$> parserLocation <*> parserLocation)

projectorParser :: Parser Projector
projectorParser =
      argreg1 ProjPrim <$> primitiveTypeParser
  <|> argreg1 ProjLabel <$> labelNameParser
  <|> requirex TokFun ProjFun

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

-- |This parser is a specialization of the @token@ parser with the necessary
--  pretty-printing and location-calculating routines embedded.
require :: forall a. (Token -> Maybe a) -> Parser (SourceRegion, a)
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
    matchToken :: SourceDocument -> PositionalToken -> Maybe (SourceRegion, a)
    matchToken doc ptok = do -- Maybe
      result <- f $ posToken ptok
      let loc = SourceRegion (textPos doc $ startPos ptok)
                             (textPos doc $ stopPos ptok)
      return (loc, result)

-- |This parser is a specialization of @require@ which demands exact token
--  equality.  If it is matched, the second argument is applied to the location
--  of the token and the result is returned.
requirex :: Token -> (SourceRegion -> a) -> Parser a
requirex t f = do
  (loc,_) <- require $ \t' -> if t == t' then Just () else Nothing
  return $ f loc
  
-- |This parser is a specialization of @requirex@ which produces a unit for all
--  matched input.
consume :: Token -> Parser ()
consume t = requirex t $ const ()
  
-- |A mechanism to retrieve the parser context.
parserContext :: Parser ParserContext
parserContext = lift ask

-- |A simple mechanism for retrieving the resource name for the current parse
--  context.
parserDocument :: Parser SourceDocument
parserDocument = lift (contextDocument <$> ask)

-- |Determines the current parser location.
parserLocation :: Parser SourceLocation
parserLocation = do
  doc <- parserDocument
  pos <- getPosition
  return $ TextSource doc (sourceLine pos) (sourceColumn pos)
  
-- |An operator which uncurries and applies a monadic operation.  It accepts a
--  non-monadic function @a -> b -> c@ and a monadic pair @m (a,b)@ and provides
--  a monadic result @m c@.
(<$&>) :: (Monad m) => (a -> b -> c) -> m (a, b) -> m c
(<$&>) f p = do
  (x,y) <- p
  return $ f x y
  
-- |A utility function to create a region from two regioned arguments.
coverRegion :: (Regioned a, Regioned b) => a -> b -> SourceRegion
coverRegion a b = SourceRegion (startLoc a) (stopLoc b)

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and one other argument; it
--  also accepts that argument.  The function is called with the region of the
--  argument.
argreg1 :: (Regioned a1) => (SourceRegion -> a1 -> r) -> a1 -> r
argreg1 f a1 = f (regionOf a1) a1

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and two other arguments; it
--  also accepts those two arguments.  The function is called with a region
--  starting at the start of the first argument and stopping at the stop of the
--  second argument.
argreg2 :: (Regioned a1, Regioned a2)
        => (SourceRegion -> a1 -> a2 -> r) -> a1 -> a2 -> r
argreg2 f a1 a2 = f (coverRegion a1 a2) a1 a2

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and three other arguments; it
--  also accepts those three arguments.  The function is called with a region
--  starting at the start of the first argument and stopping at the stop of the
--  third argument.
argreg3 :: (Regioned a1, Regioned a3)
        => (SourceRegion -> a1 -> a2 -> a3 -> r) -> a1 -> a2 -> a3 -> r
argreg3 f a1 a2 a3 = f (coverRegion a1 a3) a1 a2 a3
