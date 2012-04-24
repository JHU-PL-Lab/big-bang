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
--import Text.ParserCombinators.Parsec as P

parseTinyBang :: [L.Token] -> Either ParseError A.Expr
parseTinyBang ts = case parse parser "" ts of
    Left x -> Left (ParseError ts)
    Right x -> Right x
parser :: GenParser L.Token () A.Expr
parser = undefined


expr :: GenParser L.Token () A.Expr
expr = undefined

applExp :: GenParser L.Token () A.Expr
applExp = undefined

primary :: GenParser L.Token () A.Expr
primary = undefined

branch :: GenParser L.Token () A.Expr
branch = undefined

branches :: GenParser L.Token () A.Expr
branches = undefined

pattern :: GenParser L.Token () A.Expr
pattern = undefined
patternStruct :: GenParser L.Token () A.Expr
patternStruct = undefined
patternBind :: GenParser L.Token () A.Expr
patternBind = undefined
patternPrimary :: GenParser L.Token () A.Expr
patternPrimary = undefined
primitiveType :: GenParser L.Token () A.Expr
primitiveType = undefined
modifier :: GenParser L.Token () A.Expr
modifier = undefined
projTerm :: GenParser L.Token () A.Expr
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
