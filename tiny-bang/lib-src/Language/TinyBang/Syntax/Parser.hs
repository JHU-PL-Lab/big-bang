{-# LANGUAGE ImplicitParams #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParseError
, ParseM
) where

-- imports for ParseError
import Control.Monad.Error (ErrorT, Error, strMsg, throwError)
import Control.Monad.Identity (Identity)
import Data.Maybe (listToMaybe)

-- imports from the rest of TinyBang
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Syntax.Lexer as L
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
    ( Ident
    , ident
    , unIdent
    , LabelName
    , labelName
    , unLabelName
    , PrimitiveType(..)
    , ProjTerm(..)
    )
import Utils.Render.Display

-- imports for Parsec
import Text.ParserCombinators.Parsec (GenParser, parse)
import Text.Parsec.Combinator (optionMaybe)
import Text.Parsec.Prim (token, many)
import Text.Parsec.Pos (initialPos)
--import Text.ParserCombinators.Parsec as P

tokIdent = (flip L.TokIdentifier "")
tokCharLit = (flip L.TokCharLiteral 'x')
tokIntLit = (flip L.TokIntegerLiteral 0)

parseTinyBang :: [L.Token] -> Either ParseError A.Expr
parseTinyBang ts = case parse parser "" ts of
    Left x -> Left (ParseError ts)
    Right x -> Right x
parser :: GenParser L.Token () A.Expr
parser = undefined

isToken :: (L.SourceLocation -> L.Token) -> GenParser L.Token () L.Token
isToken t = token (show) (fst . L.getPos) isT
    where
        isT t' = if t' `L.weakEq` (t ((initialPos ""),(initialPos ""))) then Just t' else Nothing

grabIdent = do
    L.TokIdentifier _ i <- isToken tokIdent
    return i

grabInt = do
    L.TokIntegerLiteral _ v <- isToken tokIntLit
    return v

grabChar = do
    L.TokCharLiteral _ v <- isToken tokCharLit
    return v

expr :: GenParser L.Token () A.Expr
expr = undefined
    where
        lambda = do
            _ <- isToken L.TokLambda
            i <- grabIdent
            e <- expr
            return (A.Func (ident i) e)
        fun = do
            _ <- isToken L.TokFun
            i <- grabIdent
            _ <- isToken L.TokArrow
            e <- expr
            return (A.Func (ident i) e)
        defin = do
            _ <- isToken L.TokDef
            m <- optionMaybe modifier
            i <- grabIdent
            _ <- isToken L.TokOpEquals
            e1 <- expr
            _ <- isToken L.TokIn
            e2 <- expr
            return (A.Def m (ident i) e1 e2)
        assign = do
            i <- grabIdent
            _ <- isToken L.TokOpEquals
            e1 <- expr
            _ <- isToken L.TokIn
            e2 <- expr
            return (A.Assign (A.AIdent $ ident i) e1 e2)
        caseS = do
            _ <- isToken L.TokCase
            e <- expr
            _ <- isToken L.TokOf
            _ <- isToken L.TokOpenBlock
            bs <- branches
            _ <- isToken L.TokCloseBlock
            return (A.Case e bs)
        onion = do
            e1 <- expr
            _ <- isToken L.TokOnionCons
            e2 <- expr
            return (A.Onion e1 e2)
        onionsub = do
            e <- expr
            _ <- isToken L.TokOnionSub
            p <- projTerm
            return (A.OnionSub e p)
        onionproj = do
            e <- expr
            _ <- isToken L.TokOnionProj
            p <- projTerm
            return (A.OnionProj e p)
        opexp = opExp
        applexp = applExp


applExp :: GenParser L.Token () A.Expr
applExp = do
    p <- primary
    ps <- many primary
    return (foldl (A.Appl) p ps)

primary :: GenParser L.Token () A.Expr
primary = undefined
    where
        iden = do
            i <- grabIdent
            return (A.Var (ident i))
        intLit = do
            v <- grabInt
            return (A.PrimInt v)
        charLit = do
            v <- grabChar
            return (A.PrimChar v)
        unit = do
            _ <- isToken L.TokOpenParen
            _ <- isToken L.TokCloseParen
            return (A.PrimUnit)
        emptyOnion = do
            _ <- isToken L.TokOpenParen
            _ <- isToken L.TokOnionCons
            _ <- isToken L.TokCloseParen
            return (A.EmptyOnion)
        lbl = do
            _ <- isToken L.TokLabelPrefix
            m <- optionMaybe modifier
            i <- grabIdent
            p <- primary
            return (A.Label (labelName i) m p)
        expre = do
            _ <- isToken L.TokOpenParen
            e <- expr
            _ <- isToken L.TokCloseParen
            return e

branch :: GenParser L.Token () A.Branch
branch = do
    p <- pattern
    _ <- isToken L.TokArrow
    e <- expr
    return (A.Branch p e)

branches :: GenParser L.Token () A.Branches
branches = do
    b <- branch
    bs <- many branchs
    return (b:bs)
    where
        branchs :: GenParser L.Token () A.Branch
        branchs = do
            _ <- isToken L.TokSeparator
            branch

pattern :: GenParser L.Token () A.ChiMain
pattern = undefined
    where
        var = do
            i <- grabIdent
            return (A.ChiTopVar $ ident i)
        onion = do
            p1 <- patternPrimary
            _ <- isToken L.TokOnionCons
            p2 <- patternStruct
            return (A.ChiTopOnion p1 p2)
        bind = do
            p <- patternBind
            return (A.ChiTopBind p)

patternStruct :: GenParser L.Token () A.ChiStruct
patternStruct = undefined
    where
        onionMany = do
            p1 <- patternPrimary
            _ <- isToken L.TokOnionCons
            p2 <- patternStruct
            return (A.ChiOnionMany p1 p2)
        onionOne = do
            p <- patternPrimary
            return (A.ChiOnionOne p)

patternBind :: GenParser L.Token () A.ChiBind
patternBind = undefined
    where
        bound = do
            i <- grabIdent
            _ <- isToken L.TokSeparator
            p <- patternBind
            return (A.ChiBound (ident i) p)
        unbound = do
            p <- patternPrimary
            return (A.ChiUnbound p)

patternPrimary :: GenParser L.Token () A.ChiPrimary
patternPrimary = undefined
    where
        prim = do
            p <- primitiveType
            return (A.ChiPrim p)
        shallow = do
            _ <- isToken L.TokLabelPrefix
            i1 <- grabIdent
            i2 <- grabIdent
            return (A.ChiLabelShallow (labelName i1) (ident i2))
        deep = do
            _ <- isToken L.TokLabelPrefix
            i <- grabIdent
            p <- patternBind
            return (A.ChiLabelDeep (labelName i) p)
        fun = do
            _ <- isToken L.TokFun
            return A.ChiFun
        inner = do
            _ <- isToken L.TokOpenParen
            p <- patternStruct
            _ <- isToken L.TokCloseParen
            return (A.ChiInnerStruct p)

primitiveType :: GenParser L.Token () T.PrimitiveType
primitiveType = undefined
    where
        int = do
            _ <- isToken L.TokInteger
            return T.PrimInt
        char = do
            _ <- isToken L.TokChar
            return T.PrimChar
        unit = do
            _ <- isToken L.TokUnit
            return T.PrimUnit


modifier :: GenParser L.Token () A.Modifier
modifier = undefined

projTerm :: GenParser L.Token () A.ProjTerm
projTerm = undefined

opExp :: GenParser L.Token () A.Expr
opExp = undefined

op :: GenParser L.Token () A.Expr
op = undefined


data ParseError = ParseError [L.Token]
instance Error ParseError where
    strMsg = error
instance Display ParseError where
    makeDoc err =
            let desc = case err of
                        ParseError tokens ->
                            maybe "<EOS>" display $ listToMaybe tokens
            in text "unexpected" <+> text desc <+> text "token"
instance Show ParseError where
    show err = let ?conf = False in display err

type ParseM a = ErrorT ParseError Identity a

parseError :: [L.Token] -> ParseM a
parseError = throwError . ParseError
