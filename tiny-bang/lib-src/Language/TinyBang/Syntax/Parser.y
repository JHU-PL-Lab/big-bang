{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ImplicitParams #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParseError
, ParseM
) where

import Control.Monad.Error (ErrorT, runErrorT, Error, strMsg, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe (listToMaybe)

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
import Utils.Language.Ast
import Utils.Render.Display

-- For debugging purposes only
import System.IO.Unsafe
import System.IO
}

%name doParseTinyBang
%tokentype { L.Token }
%error { parseError }
%monad { ParseM } { (>>=) } { return }

%token
        '`'             { L.TokLabelPrefix }
        '&'             { L.TokOnionCons }
        '&-'            { L.TokOnionSub }
        '&.'            { L.TokOnionProj }
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
        '+'             { L.TokOpPlus }
        '-'             { L.TokOpMinus }
        '=='            { L.TokOpEquals }
        '<='            { L.TokOpLessEquals }
        '>='            { L.TokOpGreaterEquals }
        final           { L.TokFinal }
        immut           { L.TokImmut }

%left       in
%right      '->'
%left       '&' '&-' '&.'
%nonassoc   '==' '<=' '>='
%left       '+' '-'

%%

Exp     :   '\\' ident '->' Exp
                                    { astwrap $ A.Func (ident $2) $4 }
        |   fun ident '->' Exp
                                    { astwrap $ A.Func (ident $2) $4 }
        |   def ident '=' Exp in Exp
                                    { astwrap $ A.Def Nothing (ident $2) $4 $6 }
        |   def Modifier ident '=' Exp in Exp
                                    { astwrap $
                                        A.Def (Just $2) (ident $3) $5 $7 }
        |   ident '=' Exp in Exp
                                    { astwrap $
                                        A.Assign (A.AIdent $ ident $1) $3 $5 }
        |   case Exp of '{' Branches '}'
                                    { astwrap $ A.Case $2 $5 }
        |   Exp '&' Exp
                                    { astwrap $ A.Onion $1 $3 }
        |   Exp '&-' ProjTerm
                                    { astwrap $ A.OnionSub $1 $3 }
        |   Exp '&.' ProjTerm
                                    { astwrap $ A.OnionProj $1 $3 }
        |   OpExp
                                    { $1 }
        |   ApplExp
                                    { $1 }


ApplExp :   ApplExp Primary
                                    { astwrap $ A.Appl $1 $2 }
        |   Primary
                                    { $1 }


Primary :   ident
                                    { astwrap $ A.Var (ident $1) }
        |   intLit
                                    { astwrap $ A.PrimInt $1 }
        |   charLit
                                    { astwrap $ A.PrimChar $1 }
        |   '(' ')'
                                    { astwrap $ A.PrimUnit }
        |   '(' '&' ')'
                                    { astwrap $ A.EmptyOnion }
        |   '`' ident Primary
                                    { astwrap $
                                        A.Label (labelName $2) Nothing $3 }
        |   '`' Modifier ident Primary
                                    { astwrap $
                                        A.Label (labelName $3) (Just $2) $4 }
        |   '(' Exp ')'
                                    { $2 }


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
        |   '`' ident ident         { A.ChiLabelShallow (labelName $2) $
                                        ident $3 }
        |   '`' ident PatternBind   { A.ChiLabelDeep (labelName $2) $3 }
        |   fun                     { A.ChiFun }
        |   '(' PatternStruct ')'   { A.ChiInnerStruct $2 }

PrimitiveType
        :   int                     { T.PrimInt }
        |   char                    { T.PrimChar }
        |   unit                    { T.PrimUnit }

Modifier
        :   final                   { A.Final }
        |   immut                   { A.Immutable }

ProjTerm
        :   int                     { ProjPrim PrimInt }
        |   char                    { ProjPrim PrimChar }
        |   unit                    { ProjPrim PrimUnit }
        |   '`' ident               { ProjLabel (labelName $2) }
        |   fun                     { ProjFunc }

OpExp   :   Primary Op Primary      { astwrap $ $2 $1 $3 }

Op      :   '+'                     { A.LazyOp A.Plus }
        |   '-'                     { A.LazyOp A.Minus }
        |   '=='                    { A.EagerOp A.Equal }
        |   '<='                    { A.EagerOp A.LessEqual }
        |   '>='                    { A.EagerOp A.GreaterEqual }

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
    show err = let ?conf = False in display err

type ParseM a = ErrorT ParseError Identity a

parseError :: [L.Token] -> ParseM a
parseError = throwError . ParseError

parseTinyBang :: [L.Token] -> Either ParseError A.Expr
parseTinyBang tokens =
    runIdentity $ runErrorT $ doParseTinyBang tokens
}
