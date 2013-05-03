{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParserContext(..)
) where

import Control.Applicative           ((*>), (<$>), (<*), (<*>))
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Reader
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Language.TinyBang.Ast
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Parsec

{-
  NOTE: This is, in fact, mostly done.  See Utils.Tuple and Data.Concat.

  TODO: The parsing combinators that we're using here aren't good enough.  There
  is no point in parsing a branch at which the parser will commit.  This means,
  for instance, that parsing "y0: `A y1 -> y1" will result in a message like
  "unexpected :" because the body doesn't parse and so the parser tries to treat
  the whole thing like a value assignment.
  
  The solution to this is a series of combinators which are somewhat smarter
  than the simple packrat parser combinator </>.  The following combinators seem
  to be able to do the job.
  
    +++ : A concatenation combinator.  (a,b)+++(c,d,e) ==> (a,b,c,d,e).  This
          must, of course, be defined by a typeclass.  This should be fairly low
          precedence (infixl 3 or so).  We also need a variant which works under
          monads (or to overload this operator for monads) which sequences the
          values and then does the concatenation.
    ?=> : A parsing conditional operator.  a ?=> b should be equivalent to
          (try a) +++ b.  This should have low precedence as well.
    $$$ : A tuple application operator.  f $$$ (a,b,c) ==> d if f::a->b->c->d.
          This operator also needs a monadic form (or something similar to <$>).
          This should have lower precedence (e.g. infixl 2).
          
  Using this collection of operators, value parsing might look something like
  this:

    valueParser :: Parser Value
    valueParser = "Value" <@>
          argorig2 VScape $$$ patternParser <* consume TokArrow
                          ?=> consume TokOpenBrace *> expressionParser
                                <* consume TokCloseBrace
      <|> VInt <$&> require matchIntLit
      <|> VChar <$&> require matchCharLit
      <|> requirex TokEmptyOnion VEmptyOnion
      <|> argorig2 VLabel $$$ labelNameParser ?=> cellVarParser
      <|> argorig2 VOnion $$$ flowVarParser <* consume TokOnion
                          ?=> flowVarParser
      <|> argorig3 VOnionFilter $$$ flowVarParser +++ onionOpParser
                                ?=> projectorParser

  Observe that each use of ?=> appears after some token which should, by that
  point in the grammar, uniquely identify its alternative.
  
  We might also think about using typeclasses to clean up the "argorig3"
  business.
-}

-- |A function to parse TinyBang code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseTinyBang :: ParserContext -> [PositionalToken] -> Either String Expr
parseTinyBang context ts =
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
  </> argorig2 CellSet <$> cellVarParser <*> (consume TokGets *> flowVarParser)
  </> argorig2 CellGet <$> flowVarParser <*>
        (consume TokIs *> consume TokBang *> cellVarParser)
  </> argorig2 Throws <$> flowVarParser <*> (consume TokThrows *> flowVarParser)
  </> argorig2 RedexDef <$> flowVarParser <*> (consume TokIs *> redexParser)

evaluatedClauseParser :: Parser EvaluatedClause
evaluatedClauseParser = "Evaluated Clause" <@>
      argorig2 ValueDef <$> flowVarParser <*> (consume TokIs *> valueParser)
  </> argorig3 CellDef <$> cellQualifierParser <*> cellVarParser <*>
        (consume TokDef  *> flowVarParser)
  </> argorig3 Flow <$> flowVarParser <*> flowKindParser <*> flowVarParser

redexParser :: Parser Redex
redexParser = "Redex" <@>
      argorig2 Appl <$> flowVarParser <*> flowVarParser
  </> argorig3 BinOp <$> flowVarParser <*> binaryOperatorParser <*> flowVarParser
  </> argorig1 Define <$> flowVarParser

valueParser :: Parser Value
valueParser = "Value" <@>
      argorig2 VScape <$> patternParser <*> (consume TokArrow *>
        consume TokOpenBrace *> expressionParser <* consume TokCloseBrace)
  </> VInt <$&> require matchIntLit
  </> VChar <$&> require matchCharLit
  </> requirex TokEmptyOnion VEmptyOnion
  </> argorig2 VLabel <$> labelNameParser <*> cellVarParser
  </> argorig2 VOnion <$> flowVarParser <*> (consume TokOnion *> flowVarParser)
  </> argorig3 VOnionFilter <$> flowVarParser <*> onionOpParser <*> projectorParser
        
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
      argorig2 ValuePattern <$> cellVarParser <*>
        (consume TokColon *> innerPatternParser)
  </> argorig2 ValuePattern <$> (consume TokExn *> cellVarParser) <*>
        (consume TokColon *> innerPatternParser)

innerPatternParser :: Parser InnerPattern
innerPatternParser = "Inner Pattern" <@>
      argorig2 ConjunctionPattern <$> basicInnerPatternParser <*>
        (consume TokOnion *> innerPatternParser)
  </> basicInnerPatternParser
  
-- |A parser for non-onion patterns
basicInnerPatternParser :: Parser InnerPattern
basicInnerPatternParser = "Basic Inner Pattern" <@>
      consume TokOpenParen *> innerPatternParser <* consume TokCloseParen
  </> argorig1 PrimitivePattern <$> primitiveTypeParser
  -- note that this ordering makes conjunction bind more loosely than labeling;
  -- e.g. `A x:int & `B y:int is (`A x:int) & (`B y:int) and not
  -- `A x:(int & `B y:int)
  </> consume TokOpenParen *> innerPatternParser <* consume TokCloseParen
  </> argorig3 LabelPattern <$> labelNameParser <*> cellVarParser <*>
        (consume TokColon *> basicInnerPatternParser)
  </> requirex TokFun ScapePattern
  </> requirex TokEmptyOnion EmptyOnionPattern

onionOpParser :: Parser OnionOp
onionOpParser =
      requirex TokOnionSub OpOnionSub
  </> requirex TokOnionProj OpOnionProj

binaryOperatorParser :: Parser BinaryOperator
binaryOperatorParser =
      requirex TokPlus OpPlus
  </> requirex TokMinus OpMinus
  </> requirex TokEq OpEqual
  </> requirex TokLT OpLess
  </> requirex TokGT OpGreater

cellQualifierParser :: Parser CellQualifier
cellQualifierParser =
      requirex TokFinal QualFinal
  </> requirex TokImmut QualImmutable
  </> QualNone . SourceOrigin <$> (SourceRegion <$> parserLocation <*> parserLocation)

projectorParser :: Parser Projector
projectorParser =
      argorig1 ProjPrim <$> primitiveTypeParser
  </> argorig1 ProjLabel <$> labelNameParser
  </> requirex TokFun ProjFun

primitiveTypeParser :: Parser PrimitiveType
primitiveTypeParser =
      requirex TokInt PrimInt
  </> requirex TokChar PrimChar

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

argorig1 :: (Regioned a1) => (Origin -> a1 -> r) -> a1 -> r
argorig1 f = argreg1 $ \x -> f (SourceOrigin x)

argorig2 :: (Regioned a1, Regioned a2)
         => (Origin -> a1 -> a2 -> r) -> a1 -> a2 -> r
argorig2 f = argreg2 $ \x -> f (SourceOrigin x)

argorig3 :: (Regioned a1, Regioned a3)
         => (Origin -> a1 -> a2 -> a3 -> r) -> a1 -> a2 -> a3 -> r
argorig3 f = argreg3 $ \x -> f (SourceOrigin x)

-- A series of Regioned declarations for the AST types.
-- TODO: metaprogram these

-- |A typeclass for constructs containing a definitive source region.
class Regioned a where
  regionOf :: a -> SourceRegion
  startLoc :: a -> SourceLocation
  startLoc (regionOf -> SourceRegion start _) = start
  stopLoc :: a -> SourceLocation
  stopLoc (regionOf -> SourceRegion _ stop) = stop

instance Regioned Origin where
  regionOf x = case x of
    SourceOrigin reg -> reg
    _ -> error $ "Parser is working with a non-source origin: " ++ show x

instance Regioned Expr where
  regionOf x = case x of
    Expr orig _ -> regionOf orig

instance Regioned Clause where
  regionOf x = case x of
    RedexDef orig _ _ -> regionOf orig
    CellSet orig _ _ -> regionOf orig
    CellGet orig _ _ -> regionOf orig
    Throws orig _ _ -> regionOf orig
    Evaluated c -> regionOf c

instance Regioned EvaluatedClause where
  regionOf x = case x of
    ValueDef orig _ _ -> regionOf orig
    CellDef orig _ _ _ -> regionOf orig
    Flow orig _ _ _ -> regionOf orig

instance Regioned Redex where
  regionOf x = case x of
    Define orig _ -> regionOf orig
    Appl orig _ _ -> regionOf orig
    BinOp orig _ _ _ -> regionOf orig

instance Regioned Value where
  regionOf x = case x of
    VInt orig _ -> regionOf orig
    VChar orig _ -> regionOf orig
    VEmptyOnion orig -> regionOf orig
    VLabel orig _ _ -> regionOf orig
    VOnion orig _ _ -> regionOf orig
    VOnionFilter orig _ _ _ -> regionOf orig
    VScape orig _ _ -> regionOf orig

instance Regioned Pattern where
  regionOf x = case x of
    ValuePattern orig _ _ -> regionOf orig
    ExnPattern orig _ _ -> regionOf orig

instance Regioned InnerPattern where
  regionOf x = case x of
    PrimitivePattern orig _ -> regionOf orig
    LabelPattern orig _ _ _ -> regionOf orig
    ConjunctionPattern orig _ _ -> regionOf orig
    ScapePattern orig -> regionOf orig
    EmptyOnionPattern orig -> regionOf orig

instance Regioned OnionOp where
  regionOf x = case x of
    OpOnionSub orig -> regionOf orig
    OpOnionProj orig -> regionOf orig

instance Regioned BinaryOperator where
  regionOf x = case x of
    OpPlus orig -> regionOf orig
    OpMinus orig -> regionOf orig
    OpEqual orig -> regionOf orig
    OpLess orig -> regionOf orig
    OpGreater orig -> regionOf orig

instance Regioned CellQualifier where
  regionOf x = case x of
    QualFinal orig -> regionOf orig
    QualImmutable orig -> regionOf orig
    QualNone orig -> regionOf orig

instance Regioned Projector where
  regionOf x = case x of
    ProjPrim orig _ -> regionOf orig
    ProjLabel orig _ -> regionOf orig
    ProjFun orig -> regionOf orig

instance Regioned PrimitiveType where
  regionOf x = case x of
    PrimInt orig -> regionOf orig
    PrimChar orig -> regionOf orig

instance Regioned LabelName where
  regionOf x = case x of
    LabelName orig _ -> regionOf orig

instance Regioned FlowVar where
  regionOf x = case x of
    FlowVar orig _ -> regionOf orig
    GenFlowVar orig _ _ -> regionOf orig

instance Regioned CellVar where
  regionOf x = case x of
    CellVar orig _ -> regionOf orig
    GenCellVar orig _ _ -> regionOf orig
