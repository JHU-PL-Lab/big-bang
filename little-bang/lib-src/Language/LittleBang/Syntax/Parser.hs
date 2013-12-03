{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns,
             TemplateHaskell #-}

module Language.LittleBang.Syntax.Parser
( parseLittleBang
, ParserContext(..)
) where

import Control.Applicative           ((*>), (<$>),(<$),(*>) ,(<*), (<*>))
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Reader
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Data.List

import Utils.ParserUtils
import Language.TinyBang.Ast.Data (Origin (..))
import Language.TinyBang.Display
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Parsec
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Ast.Data

-- |A function to parse LittleBang code tokens into an @Expr@.  If this is
--  successful, the result is a right @Expr@; otherwise, the result is a left
--  error message.
parseLittleBang :: ParserContext -> [PositionalToken] -> Either String Expr
parseLittleBang context ts =
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

-- |A mechanism to retrieve the parser context.
parserContext :: Parser ParserContext
parserContext = lift ask

-- |A simple mechanism for retrieving the resource name for the current parse
--  context.
parserDocument :: Parser SourceDocument  
parserDocument = lift (contextDocument <$> ask)


-- |Begin parser. Expr-Expr7 handle expressions, OuterPattern and Pattern parse patterns,
-- |and the remaining parsers handle specific language elements, i.e. labels and vars

-- |A program is parsed as a single expression
programParser :: Parser Expr
programParser = expressionParser <* eof

-- Expr ::= def Var = Expr in Expr
-- Expr ::= Var = Expr in Expr
-- Expr ::= Expr1
expressionParser :: Parser Expr
expressionParser = 
      argorig3 ExprDef <$ consume TokDef <*> varParser <* consume TokIs <*> expressionParser <* consume TokIn <*> expressionParser
  <|> argorig3 ExprVarIn <$> varParser <* consume TokIs <*> expressionParser <* consume TokIn ?=> expressionParser
  <|> scapeExprParser
  <?> "top level expression"

-- Expr1 ::= OuterPattern -> Expr1
-- Expr1 ::= Expr2
scapeExprParser :: Parser Expr
scapeExprParser = 
      argorig2 ExprScape <$> outerPatternParser <* consume TokArrow ?=> scapeExprParser
  <|> conditionParser
  <?> "scape expression"
          
-- Expr2 ::= if Expr2 then Expr1 else Expr1
-- Expr2 ::= Expr3          
conditionParser :: Parser Expr
conditionParser = 
      argorig3 ExprCondition <$ consume TokIf <*> conditionParser 
                <* consume TokThen <*> scapeExprParser <* consume TokElse <*> scapeExprParser 
  <|> arithOpExprParser  
  <?> "condition expression"        
          
          
-- Expr3 ::= Expr4 ArithOp Expr3
-- Expr3 ::= Expr4
arithOpExprParser :: Parser Expr
arithOpExprParser = 
      chainl1 onionOpExprParser (buildExpr <$> arithOpParser)
  <|> onionOpExprParser
  <?> "arithOp expression"
         where
           buildExpr :: BinaryOperator -> Expr -> Expr -> Expr
           buildExpr a e1 e2 = argorig3 ExprBinaryOp e1 a e2
                    
-- Expr4 ::= Expr5 (OnionOp Projector)*
-- Expr4 ::= Expr5
onionOpExprParser :: Parser Expr
onionOpExprParser = 
      try ( do { e <-  onionExprParser  -- get Expr4
               ; opProjPairList <- many1 parseOpProjPair -- all OnionOp Proj pairs
               ; return $ foldl' combinePairs e opProjPairList -- fold into resulting Expr
               }
          )
  <|> onionExprParser
  <?> "onionop expression"
        where
          parseOpProjPair :: Parser (OnionOperator, Projector)
          parseOpProjPair = try ( do { op <- onionOpParser
                                     ; proj <- projectorParser
                                     ; return (op, proj)
                                     }
                                )
          combinePairs :: Expr -> (OnionOperator, Projector) -> Expr
          combinePairs e (o, p)= argorig3 ExprOnionOp e o p

-- Expr5 ::= Expr5 & Expr6
-- Expr5 ::= Expr6
onionExprParser :: Parser Expr
onionExprParser = 
      chainl1 applExprParser (buildExpr <$ (consume TokOnion))
  <|> applExprParser
  <?> "onion expression"
        where
          buildExpr :: Expr -> Expr -> Expr
          buildExpr e1 e2 = argorig2 ExprOnion e1 e2

-- Expr6 ::= Expr6 Expr7
-- Expr6 ::= Expr7
applExprParser :: Parser Expr
applExprParser = 
      chainl1 labelExprParser (return buildExpr)
  <|> labelExprParser
  <?> "application expression"                 
        where 
          buildExpr :: Expr -> Expr -> Expr
          buildExpr e1 e2 = argorig2 ExprAppl e1 e2
                       
-- Expr7 ::= Label Expr7
-- Expr7 ::= Expr8
labelExprParser :: Parser Expr
labelExprParser = 
      argorig2 ExprLabelExp <$> labelParser ?=> labelExprParser
  <|> valExprParser
  <?> "label expression"
              
-- Expr8 ::= Var
-- Expr8 ::= Integer
-- Expr8 ::= Char
-- Expr8 ::= Unit
-- Expr8 ::= ( Expr )
valExprParser :: Parser Expr
valExprParser = 
      ExprValInt <$&> require matchIntLit
  <|> ExprValChar <$&> require matchCharLit                
  <|> requirex TokEmptyOnion ExprValUnit             
  <|> argorig1 ExprVar <$> varParser
  <|> consume TokOpenParen *> expressionParser <* consume TokCloseParen
  <?> "value expression - int/char literal, (), variable or use parens"
            
-- OuterPattern ::= Var : Pattern
-- OuterPattern ::= Pattern (removed)
outerPatternParser :: Parser OuterPattern
outerPatternParser = 
      argorig2 OuterPatternLabel <$> varParser <* consume TokColon ?=> patternOnionParser
  <?> "outer pattern - \"Var : Pattern\""

-- Pattern ::= Pattern & Pattern1
-- Pattern ::= Pattern1
patternOnionParser :: Parser Pattern
patternOnionParser = 
      chainl1 patternLabelParser (buildExpr <$ (consume TokOnion))
  <|> patternLabelParser
  <?> "onion pattern"
        where
          buildExpr :: Pattern -> Pattern -> Pattern
          buildExpr p1 p2 = argorig2 ConjunctionPattern p1 p2
                  
-- Pattern1 ::= Label Var : Pattern1
-- Pattern1 ::= Pattern2
patternLabelParser :: Parser Pattern
patternLabelParser = 
      argorig3 LabelPattern <$> labelParser <*> varParser <* consume TokColon <*> patternPrimParser 
  <|> patternPrimParser
  <?> "label pattern"


-- Pattern2 ::= Primitive
-- Pattern2 ::= fun
-- Pattern2 ::= Var (removed)
-- Pattern2 ::= (Pattern)
patternPrimParser :: Parser Pattern
patternPrimParser = 
      argorig1 PrimitivePattern <$> primitiveParser
  <|> requirex TokFun ScapePattern
  <|> requirex TokEmptyOnion EmptyOnionPattern
  <|> consume TokOpenParen *> patternOnionParser <* consume TokCloseParen
  <?> "primitive pattern"

primitiveParser :: Parser Primitive
primitiveParser = 
      requirex TokInt TInt
  <|> requirex TokChar TChar
  <?> "primitive"

arithOpParser :: Parser BinaryOperator
arithOpParser = 
      requirex TokPlus OpPlus
  <|> requirex TokMinus OpMinus 
  <|> requirex TokEq OpEqual
  <|> requirex TokGT OpGreater 
  <|> requirex TokLT OpLesser
  <|> requirex TokGTE OpGreaterEq 
  <|> requirex TokLTE OpLesserEq
  <?> "arithmetic op"

onionOpParser :: Parser OnionOperator
onionOpParser = 
      requirex TokOnionSub OpOnionSub
  <|> requirex TokOnionProj OpOnionProj
  <?> "onion operator"

varParser :: Parser Var
varParser = 
      Var <$&> require matchIdent
  <?> "variable identifier"

labelParser :: Parser Label
labelParser = 
      LabelDef <$&> require matchLabel
  <?> "label identifier"

projectorParser :: Parser Projector
projectorParser = 
      requirex TokFun FunProjector
  <|> argorig1 LabelProjector <$> labelParser
  <|> argorig1 PrimitiveProjector <$> primitiveParser
  <?> "projector: int, char, (), fun or label"

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

