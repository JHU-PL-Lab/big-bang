{-# LANGUAGE  ImplicitParams
            , FlexibleContexts
            #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParseError
)
where

-- imports from the rest of TinyBang
import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Ast (Pattern(..), PrimaryPattern(..), ProjTerm(..))
import qualified Language.TinyBang.Syntax.Lexer as L
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
    ( ident
    , labelName
    , LabelName
    )
import Utils.Language.Ast
import Utils.Render.Display

-- imports for Parsec
import Text.Parsec ( token
                   , many
                   , (<|>)
                   , try
                   , optionMaybe
                   , eof
                   , option
                   , parse
                   , ParseError
                   , Parsec
                   , choice
                   , sepBy
                   , between
                   , many1)

import Control.Applicative ((<$>))

tokIdent   :: L.RawToken
tokIdent   =  L.TokIdentifier $ error "tokIdent's payload examined"
tokCharLit :: L.RawToken
tokCharLit =  L.TokCharLiteral $ error "tokCharLit's payload examined"
tokIntLit  :: L.RawToken
tokIntLit  =  L.TokIntegerLiteral $ error "tokIntLit's payload examined"

type TokParser = Parsec [L.Token] ()

-- TODO: refactor to use Parsec's error messages in some way
parseTinyBang :: [L.Token] -> Either ParseError A.Expr
parseTinyBang ts =  case parse parser "" ts of
    Left x -> Left x
    Right x -> Right x
parser :: TokParser A.Expr
parser = do
    e <- expr
    eof --must use all the tokens in the stream
    return e

-- I don't understand how (fst . L.getPos) does what is expected in
-- `token`'s contract, but I'll leave it as is.
isToken :: L.RawToken -> TokParser L.Token
isToken t = token show (fst . L.getPos) isT
    where
        isT t' = if t' `L.weaklyMatchesRawToken` t then Just t' else Nothing

grabIdent :: TokParser String
grabIdent = do
    L.TokIdentifier i <- L.getRawToken <$> isToken tokIdent
    return i

grabInt :: TokParser Integer
grabInt = do
    L.TokIntegerLiteral v <- L.getRawToken <$> isToken tokIntLit
    return v

grabChar :: TokParser Char
grabChar = do
    L.TokCharLiteral v <- L.getRawToken <$> isToken tokCharLit
    return v

parseLabelName :: TokParser LabelName
parseLabelName =  isToken L.TokLabelPrefix >> labelName <$> grabIdent

expr :: TokParser A.Expr
expr = do
    e <- exprBasic
    option e $ exprRest e

exprBasic :: TokParser A.Expr
exprBasic = choice $ map try $
  [ scape
  , defin
  , assign
  , opexp
  , applexp
  ]
  where scape = do
            p <- pattern
            _ <- isToken L.TokArrow
            e <- expr
            return (astwrap $ A.Scape p e)
        defin = do
            _ <- isToken L.TokDef
            m <- optionMaybe modifier
            i <- grabIdent
            _ <- isToken L.TokEquals
            e1 <- expr
            _ <- isToken L.TokIn
            e2 <- expr
            return (astwrap $ A.Def m (ident i) e1 e2)
        assign = do
            i <- grabIdent
            _ <- isToken L.TokEquals
            e1 <- expr
            _ <- isToken L.TokIn
            e2 <- expr
            return (astwrap $ A.Assign (ident i) e1 e2)
        opexp = opExp
        applexp = applExp

exprRest :: A.Expr -> TokParser A.Expr
exprRest e1 = do
    os <- many onionPart
    return $ foldl (\e f -> f e) e1 os

onionPart :: TokParser (A.Expr -> A.Expr)
onionPart = choice
  [ onion
  , onionsub
  , onionproj
  ]
  where onion = do
            --e1 <- expr
            _ <- isToken L.TokOnionCons
            e2 <- exprBasic
            return (astwrap . flip A.Onion e2)
        onionsub = do
            --e1 <- expr
            _ <- isToken L.TokOnionSub
            p <- projTerm
            return (astwrap . flip A.OnionSub p)
        onionproj = do
            --e1 <- expr
            _ <- isToken L.TokOnionProj
            p <- projTerm
            return (astwrap . flip A.OnionProj p)

applExp :: TokParser A.Expr
applExp = try primaries
    where
        primaries = do
            ps <- many1 primary
            return (foldl1 (binwrap A.Appl) ps)

primary :: TokParser A.Expr
primary = choice $ map try $
  [ iden
  , intLit
  , charLit
  , unit
  , emptyOnion
  , lbl
  , expre
  ]
  where iden = do
            i <- grabIdent
            return $ astwrap $ A.Var (ident i)
        intLit = do
            v <- grabInt
            return $ astwrap $ A.PrimInt v
        charLit = do
            v <- grabChar
            return $ astwrap $ A.PrimChar v
        unit = do
            _ <- isToken L.TokOpenParen
            _ <- isToken L.TokCloseParen
            return $ astwrap A.PrimUnit
        emptyOnion = do
            _ <- isToken L.TokOpenParen
            _ <- isToken L.TokOnionCons
            _ <- isToken L.TokCloseParen
            return $ astwrap A.EmptyOnion
        lbl = do
            _ <- isToken L.TokLabelPrefix
            m <- optionMaybe modifier
            i <- grabIdent
            p <- primary
            return $ astwrap $ A.Label (labelName i) m p
        expre = between
                  (isToken L.TokOpenParen)
                  (isToken L.TokCloseParen)
                  expr

pattern :: TokParser Pattern
pattern = bound <|> Pattern (ident "_") <$> primaryPattern
    where bound = do
            i <- grabIdent
            pp <- option (PatOnion []) $ isToken L.TokColon >> primaryPattern
            return $ Pattern (ident i) pp

primaryPattern :: TokParser PrimaryPattern
primaryPattern = choice
  [ PatPrim <$> primitiveType
  , label
  , onion
  , isToken L.TokFun >> return PatFun
  ]
  where label = do
          l <- parseLabelName
          Pattern i pp <- pattern
          return $ PatLabel l i pp
        onion = PatOnion <$> between
                  (isToken L.TokOpenParen)
                  (isToken L.TokCloseParen)
                  primaryPattern `sepBy` isToken L.TokOnionCons

primitiveType :: TokParser T.PrimitiveType
primitiveType = choice [pint, pchar, punit]
    where
        pint = isToken L.TokInteger >> return T.PrimInt
        pchar = isToken L.TokChar >> return T.PrimChar
        punit = isToken L.TokUnit >> return T.PrimUnit

modifier :: TokParser A.Modifier
modifier = try final <|> try immut
    where
        final = isToken L.TokFinal >> return A.Final
        immut = isToken L.TokImmut >> return A.Immutable

projTerm :: TokParser A.ProjTerm
projTerm = choice $ map try $
  [ pint
  , pchar
  , punit
  , plbl
  , pfun
  ]
  where pint = isToken L.TokInteger >> return (ProjPrim T.PrimInt)
        pchar = isToken L.TokChar >> return (ProjPrim T.PrimChar)
        punit = isToken L.TokUnit >> return (ProjPrim T.PrimUnit)
        plbl = ProjLabel <$> parseLabelName
        pfun = isToken L.TokFun >> return ProjFunc

opExp :: TokParser A.Expr
opExp = do
    p1 <- primary
    o <- op
    p2 <- primary
    return (o p1 p2)

op :: TokParser (A.Expr -> A.Expr -> A.Expr)
op = choice $ map try $
  [ plus
  , minus
  , equal
  , lessEqual
  , greaterEqual
  ]
  where plus = isToken L.TokOpPlus >> return (binwrap $ A.LazyOp A.Plus)
        minus = isToken L.TokOpMinus >> return (binwrap $ A.LazyOp A.Minus)
        equal = isToken L.TokOpEquals >> return (binwrap $ A.EagerOp A.Equal)
        lessEqual = isToken L.TokOpLessEquals >>
                    return (binwrap $ A.EagerOp A.LessEqual)
        greaterEqual = isToken L.TokOpGreaterEquals >>
                       return (binwrap $ A.EagerOp A.GreaterEqual)

instance Display ParseError where
    makeDoc err = text (show err)

binwrap :: (AstWrap A.ExprPart ast)
        => (ast -> ast -> A.ExprPart ast) -> (ast -> ast -> ast)
binwrap f = \x y -> astwrap $ f x y
