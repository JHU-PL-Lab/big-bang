{
{-# OPTIONS_GHC -w #-}

module Language.MicroBang.Syntax.Parser
( parseMicroBang
, ParseError
, ParseM
) where

import Control.Monad.Error (ErrorT, runErrorT, Error, strMsg, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (listToMaybe)

import qualified Language.MicroBang.Ast as A
import qualified Language.MicroBang.Syntax.Lexer as L
import Language.MicroBang.Types.UtilTypes as T
import Language.MicroBang.Types.UtilTypes
    ( Ident
    , ident
    , unIdent
    , labelName
    , unLabelName
    )
import Utils.Render.Display

-- For debugging purposes only
import System.IO.Unsafe
import System.IO
}

%name doParseMicroBang
%tokentype { L.Token }
%error { parseError }
%monad { ParseM } { (>>=) } { return }

%token
        '`'             { L.TokLabelPrefix }
        '&'             { L.TokOnionCons }
        fun             { L.TokFun }
        '->'            { L.TokArrow }
        case            { L.TokCase }
        of              { L.TokOf }
        int             { L.TokInteger }
        unit            { L.TokUnit }
        '('             { L.TokOpenParen }
        ')'             { L.TokCloseParen }
        intLit          { L.TokIntegerLiteral $$ }
        ident           { L.TokIdentifier $$ }
        '{'             { L.TokOpenBlock }
        '}'             { L.TokCloseBlock }
        ';'             { L.TokSeparator }
        '_'             { L.TokUnder }
        ':'             { L.TokColon }
        '[+]'           { L.TokOpPlus }
        '[=]'           { L.TokOpEquals }
        '-int'          { L.TokSubInteger }
        '-unit'         { L.TokSubUnit }
        '-`'            { L.TokSubLabelPrefix }
        '-fun'          { L.TokSubFun }

%left       in
%right      '->'
%left       '&'

%%

Exp     :   fun ident '->' Exp
                                    { A.Func (ident $2) $4 }
        |   case Exp of '{' Branches '}'
                                    { A.Case $2 $5 }
        |   Exp '&' Exp
                                    { A.Onion $1 $3 }
        |   Exp '&' SubTerm
                                    { A.OnionSub $1 $3 }
        |   OpExp
                                    { $1 }
        |   ApplExp
                                    { $1 }


ApplExp :   ApplExp Primary
                                    { A.Appl $1 $2 }
        |   Primary
                                    { $1 }


Primary :   ident
                                    { A.Var (ident $1) }
        |   intLit
                                    { A.PrimInt $1 }
        |   '(' ')'
                                    { A.PrimUnit }
        |   '`' ident Primary
                                    { A.Label (labelName $2) $3 }
        |   '(' Exp ')'
                                    { $2 }


Branches:   Branch ';' Branches     { $1:$3 }
        |   Branch                  { [$1] }


Branch  :   Pattern '->' Exp
                                    { A.Branch Nothing $1 $3 }
        |   ident ':' Pattern '->' Exp
                                    { A.Branch (Just $ ident $1) $3 $5 }


Pattern :   PrimitiveType           { A.ChiPrim $1 }
        |   '`' ident ident         { A.ChiLabel (labelName $2) (ident $3) }
        |   fun                     { A.ChiFun }
        |   '_'                     { A.ChiAny }

PrimitiveType
        :   int                     { T.PrimInt }
        |   unit                    { T.PrimUnit }

SubTerm :   '-int'                  { T.SubPrim PrimInt }
        |   '-unit'                 { T.SubPrim PrimUnit }
        |   '-`' ident              { T.SubLabel (labelName $2) }
        |   '-fun'                  { T.SubFunc }

OpExp   :   Op Primary Primary      { $1 $2 $3 }

Op      :   '[+]'                   { \x y -> A.BinOp A.Plus x y }
        |   '[=]'                   { \x y -> A.BinOp A.Equal x y }

{
data ParseError
        = ParseError [L.Token]
instance Error ParseError where
    strMsg = error
instance Display ParseError where
    makeDoc err =
            let desc = case err of
                        ParseError tokens ->
                            maybe "<EOS>" display $ listToMaybe tokens
            in text "unexpected" <+> text desc <+> text "token"
instance Show ParseError where
    show = display

type ParseM a = ErrorT ParseError Identity a

parseError :: [L.Token] -> ParseM a
parseError = throwError . ParseError

parseMicroBang :: [L.Token] -> Either ParseError A.Expr
parseMicroBang tokens =
    runIdentity $ runErrorT $ doParseMicroBang tokens
}
