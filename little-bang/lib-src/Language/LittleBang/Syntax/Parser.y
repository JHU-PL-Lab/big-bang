{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ImplicitParams #-}

module Language.LittleBang.Syntax.Parser
( parseLittleBang
, ParseError
, ParseM
) where

import Control.Monad.Error (ErrorT, runErrorT, Error, strMsg, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (listToMaybe)

import qualified Language.LittleBang.Ast as A
import qualified Language.LittleBang.Syntax.Lexer as L
import Language.LittleBang.Types.UtilTypes
    ( Ident
    , ident
    , unIdent
    , LabelName
    , labelName
    , unLabelName
    , PrimitiveType(..)
    , SubTerm(..)
    )
import Utils.Render.Display

-- For debugging purposes only
import System.IO.Unsafe
import System.IO
}

%name doParseLittleBang
%tokentype { L.Token }
%error { parseError }
%monad { ParseM } { (>>=) } { return }

%token
        '`'             { L.TokLabelPrefix }
        '&'             { L.TokOnionCons }
        '\\'            { L.TokLambda }
        fun             { L.TokFun }
        '->'            { L.TokArrow }
        case            { L.TokCase }
        of              { L.TokOf }
        int             { L.TokInteger }
        char            { L.TokChar }
        unit            { L.TokUnit }
        '('             { L.TokOpenParen }
        ')'             { L.TokCloseParen }
        intLit          { L.TokIntegerLiteral $$ }
        charLit         { L.TokCharLiteral $$ }
        ident           { L.TokIdentifier $$ }
        '{'             { L.TokOpenBlock }
        '}'             { L.TokCloseBlock }
        ';'             { L.TokSeparator }
        ':'             { L.TokColon }
        def             { L.TokDef }
        '='             { L.TokEquals }
        in              { L.TokIn }
        '[+]'           { L.TokOpPlus }
        '[-]'           { L.TokOpMinus }
        '[=]'           { L.TokOpEquals }
        '[<=]'          { L.TokOpLessEquals }
        '[>=]'          { L.TokOpGreaterEquals }
        '-int'          { L.TokSubInteger }
        '-char'         { L.TokSubChar }
        '-unit'         { L.TokSubUnit }
        '-`'            { L.TokSubLabelPrefix }
        '-fun'          { L.TokSubFun }
        self            { L.TokSelf }

%left       in
%right      '->'
%left       '&'

%%

Exp     :   '\\' ident '->' Exp
                                    { A.Func (ident $2) $4 }
        |   fun ident '->' Exp
                                    { A.Func (ident $2) $4 }
        |   def ident '=' Exp in Exp
                                    { A.Def (ident $2) $4 $6 }
        |   ident '=' Exp in Exp
                                    { A.Assign (ident $1) $3 $5 }
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
        |   charLit
                                    { A.PrimChar $1 }
        |   '(' ')'
                                    { A.PrimUnit }
        |   '(' '&' ')'
                                    { A.EmptyOnion }
        |   '`' ident Primary
                                    { A.Label (labelName $2) $3 }
        |   '(' Exp ')'
                                    { $2 }
        |   self
                                    { A.Self }


Branches:   Branch ';' Branches     { $1:$3 }
        |   Branch                  { [$1] }


Branch  :   Pattern '->' Exp        { A.Branch $1 $3 }


Pattern :   ident                  
                                    { A.ChiTopVar $ ident $1 }
        |   PatternPrimary '&' PatternStruct
                                    { A.ChiTopOnion $1 $3 }
        |   PatternBind
                                    { A.ChiTopBind $1 }

PatternStruct
        :   PatternPrimary '&' PatternStruct
                                    { A.ChiOnionMany $1 $3 }
        |   PatternPrimary
                                    { A.ChiOnionOne $1 }

PatternBind
        :   ident ':' PatternBind   { A.ChiBound (ident $1) $3 }
        |   PatternPrimary          { A.ChiUnbound $1 }

PatternPrimary
        :   PrimitiveType           { A.ChiPrim $1 }
        |   '`' ident ident         { A.ChiLabelShallow (labelName $2) $ ident $3 }
        |   '`' ident PatternBind   { A.ChiLabelDeep (labelName $2) $3 }
        |   fun                     { A.ChiFun }
        |   '(' PatternStruct ')'   { A.ChiInnerStruct $2 }

PrimitiveType
        :   int                     { PrimInt }
        |   char                    { PrimChar }
        |   unit                    { PrimUnit }

SubTerm :   '-int'                  { SubPrim PrimInt }
        |   '-char'                 { SubPrim PrimChar }
        |   '-unit'                 { SubPrim PrimUnit }
        |   '-`' ident              { SubLabel (labelName $2) }
        |   '-fun'                  { SubFunc }

OpExp   :   Op Primary Primary      { $1 $2 $3 }

Op      :   '[+]'                   { \x y -> A.BinOp A.Plus x y }
        |   '[-]'                   { \x y -> A.BinOp A.Minus x y }
        |   '[=]'                   { \x y -> A.BinOp A.Equal x y }
        |   '[<=]'                  { \x y -> A.BinOp A.LessEqual x y }
        |   '[>=]'                  { \x y -> A.BinOp A.GreaterEqual x y }

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
    show err = let ?debug = False in display err

type ParseM a = ErrorT ParseError Identity a

parseError :: [L.Token] -> ParseM a
parseError = throwError . ParseError

parseLittleBang :: [L.Token] -> Either ParseError A.Expr
parseLittleBang tokens =
    runIdentity $ runErrorT $ doParseLittleBang tokens
}
