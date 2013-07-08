{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns,
             TemplateHaskell #-}

module Language.PatBang.Syntax.Parser
( parsePatBang
, ParserContext(..)
) where

import Control.Applicative           ((*>), (<$>), (<$), (<*), (<*>))
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Reader
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.Syntax.Lexer
import Language.PatBang.Syntax.Location
import Language.PatBang.Utils.Parsec

$(loggingFunctions)

-- |A function to parse PatBang code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parsePatBang :: ParserContext -> [PositionalToken] -> Either String Expr
parsePatBang context ts =
  _debugI ("Parsing PatBang source with tokens: " ++
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

-- |A type alias for the PatBang parser.
type Parser a = ParsecT [PositionalToken] () ParserMonad a

programParser :: Parser Expr
programParser = expressionParser <* eof

expressionParser :: Parser Expr
expressionParser = "Expression" <@> do
  cls <- sepBy1 clauseParser $ consume TokSemi
  return $ Expr (SourceOrigin $ coverRegion (head cls) (last cls)) cls

clauseParser :: Parser Clause
clauseParser = "Clause" <@>
      argorig2 ValueDef <$> flowVarParser <* consume TokIs <*> valueParser
        ?+> eps
  <|> argorig2 RedexDef <$> flowVarParser <* consume TokIs ?=> redexParser

redexParser :: Parser Redex
redexParser = "Redex" <@>
      argorig2 Appl <$> flowVarParser <*> flowVarParser ?+> eps
  <|> argorig3 BinOp <$> flowVarParser <*> binaryOperatorParser
                          ?=> flowVarParser
  <|> argorig1 Define <$> flowVarParser ?+> eps

valueParser :: Parser Value
valueParser = "Value" <@>
      argorig2 VFunction <$> astListParser flowVarParser <* consume TokArrow ?+>
        consume TokOpenBrace <*> expressionParser <* consume TokCloseBrace
  <|> argorig2 VPattern <$> astListParser patVarParser <* consume TokBArrow ?+>
        consume TokOpenBrace <*> patternBodyParser <* consume TokCloseBrace
  <|> argorig2 VScape <$> flowVarParser <* consume TokJoin ?=> flowVarParser
  <|> VInt <$&> require matchIntLit
  <|> requirex TokEmptyOnion VEmptyOnion
  <|> argorig2 VLabel <$> labelNameParser ?=> flowVarParser
  <|> argorig2 VOnion <$> flowVarParser <* consume TokAmp ?=> flowVarParser
  
astListParser :: Parser a -> Parser (AstList a)
astListParser p = do
  start <- parserLocation
  consume TokOpenParen
  els <- p `sepBy` consume TokComma
  consume TokCloseParen
  stop <- parserLocation
  return $ AstList (SourceOrigin $ SourceRegion start stop) els
  
{-
  Pattern parse order:
    * recursive patterns
    * conjunctive patterns
    * label patterns
    * everything else (including parentheticals)
-}

patternBodyParser :: Parser PatternBody
patternBodyParser = "Pattern Body" <@>
      argorig2 PRec <$ consume TokRec ?=> patVarParser <* consume TokColon
        <*> patternBodyParser
  <|> patternBody1Parser
  
patternBody1Parser :: Parser PatternBody
patternBody1Parser = "Pattern Body (1)" <@>
      argorig2 PConj <$> patternBody2Parser <* consume TokAmp
        ?=> patternBody1Parser
  <|> patternBody2Parser
  
patternBody2Parser :: Parser PatternBody
patternBody2Parser = "Pattern Body (2)" <@>
      argorig2 PLabel <$> labelNameParser ?=> patternBody2Parser
  <|> patternBody3Parser

patternBody3Parser :: Parser PatternBody
patternBody3Parser = "Pattern Body (3)"  <@>
      argorig1 PPrim <$> primitiveTypeParser ?+> eps
  <|> requirex TokFun PFun
  <|> requirex TokPat PPat
  <|> requirex TokScape PScape
  <|> argorig2 PSubst <$> flowVarParser <*> astListParser patternBodyParser
        ?+> eps
  <|> argorig1 PVar <$> patVarParser
  <|> consume TokOpenParen *> patternBodyParser <* consume TokCloseParen

binaryOperatorParser :: Parser BinaryOperator
binaryOperatorParser =
      requirex TokPlus OpPlus
  <|> requirex TokMinus OpMinus
  <|> requirex TokEq OpEqual
  <|> requirex TokLT OpLess
  <|> requirex TokGT OpGreater

primitiveTypeParser :: Parser PrimitiveType
primitiveTypeParser = requirex TokInt PrimInt

labelNameParser :: Parser LabelName
labelNameParser = "Label Name" <@> LabelName <$&> require matchLabel

flowVarParser :: Parser FlowVar
flowVarParser = "Flow Variable" <@> FlowVar <$&> require matchIdent

patVarParser :: Parser PatVar
patVarParser = "Pattern Variable" <@> PatVar <$&> require matchIdent

-- |A matching function for integer literals.
matchIntLit :: Token -> Maybe Integer
matchIntLit x =
  case x of
    TokLitInt n -> Just n
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
  
-- |This parser is a specialization of @requirex@ which produces a unit for all
--  matched input.
consume :: Token -> Parser ()
consume t = display t <@> requirex t (const ())
  
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
  
-- |A utility function to create a region from two HasOrigin arguments.
coverRegion :: (Display a, Display b, HasOrigin a, HasOrigin b)
            => a -> b -> SourceRegion
coverRegion a b = SourceRegion (startLoc a) (stopLoc b)

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and one other argument; it
--  also accepts that argument.  The function is called with the region of the
--  argument.
argreg1 :: (Display a1, HasOrigin a1) => (SourceRegion -> a1 -> r) -> a1 -> r
argreg1 f a1 = f (regionOf a1) a1

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and two other arguments; it
--  also accepts those two arguments.  The function is called with a region
--  starting at the start of the first argument and stopping at the stop of the
--  second argument.
argreg2 :: (Display a1, Display a2, HasOrigin a1, HasOrigin a2)
        => (SourceRegion -> a1 -> a2 -> r) -> a1 -> a2 -> r
argreg2 f a1 a2 = f (coverRegion a1 a2) a1 a2

-- |A wrapper to automatically extract source regions.  This function accepts
--  another function which expects a source region and three other arguments; it
--  also accepts those three arguments.  The function is called with a region
--  starting at the start of the first argument and stopping at the stop of the
--  third argument.
argreg3 :: (Display a1, Display a3, HasOrigin a1, HasOrigin a3)
        => (SourceRegion -> a1 -> a2 -> a3 -> r) -> a1 -> a2 -> a3 -> r
argreg3 f a1 a2 a3 = f (coverRegion a1 a3) a1 a2 a3

argorig1 :: (Display a1, HasOrigin a1) => (Origin -> a1 -> r) -> a1 -> r
argorig1 f = argreg1 $ \x -> f (SourceOrigin x)

argorig2 :: (Display a1, Display a2, HasOrigin a1, HasOrigin a2)
         => (Origin -> a1 -> a2 -> r) -> a1 -> a2 -> r
argorig2 f = argreg2 $ \x -> f (SourceOrigin x)

argorig3 :: (Display a1, Display a3, HasOrigin a1, HasOrigin a3)
         => (Origin -> a1 -> a2 -> a3 -> r) -> a1 -> a2 -> a3 -> r
argorig3 f = argreg3 $ \x -> f (SourceOrigin x)

-- |Extracts the region from an entity which has a source origin.  If
--  the argument has a non-source origin, this function will error.  All AST
--  nodes generated by the parser have source origins.
regionOf :: (Display a, HasOrigin a) => a -> SourceRegion
regionOf x = case originOf x of
  SourceOrigin region -> region
  _ -> error $ "regionOf called on non-source origin " ++ display x

-- |Extracts the start location from an entity which has a source origin.  If
--  the argument has a non-source origin, this function will error.  All AST
--  nodes generated by the parser have source origins.
startLoc :: (Display a, HasOrigin a) => a -> SourceLocation
startLoc (regionOf -> SourceRegion start _) = start 

-- |Extracts the stop location from an entity which has a source origin.  If
--  the argument has a non-source origin, this function will error.  All AST
--  nodes generated by the parser have source origins.
stopLoc :: (Display a, HasOrigin a) => a -> SourceLocation
stopLoc (regionOf -> SourceRegion _ stop) = stop
