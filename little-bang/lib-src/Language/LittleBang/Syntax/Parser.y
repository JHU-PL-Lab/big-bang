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

import qualified Language.LittleBang.Ast as LA
import qualified Language.LittleBang.Syntax.Lexer as L
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
import qualified Language.TinyBang.Ast as TA
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
        '&-'            { L.TokOnionSub }
        '&.'            { L.TokOnionProj }
        '->'            { L.TokArrow }
        case            { L.TokCase }
        of              { L.TokOf }
        int             { L.TokInteger }
        char            { L.TokChar }
        unit            { L.TokUnit }
        any             { L.TokAny }
        fun             { L.TokFun }
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
        '.'             { L.TokProj }
        self            { L.TokSelf }
        prior           { L.TokPrior }

%left       in
%right      '->'
%left       '&' '&-' '&.'
%nonassoc   '==' '<=' '>='
%left       '+' '-'
%left       '.'

%%

Exp     :   Pattern '->' Exp
                                    { LA.scape $1 $3 }
        |   def ident '=' Exp in Exp
                                    { TA.def Nothing (ident $2) $4 $6 }
        |   def Modifier ident '=' Exp in Exp
                                    { TA.def (Just $2) (ident $3) $5 $7 }
        |   ident '=' Exp in Exp
                                    { TA.assign (ident $1) $3 $5 }
        |   Primary '.' ident '=' Exp in Exp
                                    { LA.projAssign $1 (ident $3) $5 $7 }
        |   Exp '&' Exp
                                    { LA.onion $1 $3 }
        |   Exp '&-' ProjTerm
                                    { TA.onionSub $1 $3 }
        |   Exp '&.' ProjTerm
                                    { TA.onionProj $1 $3 }
        |   OpExp
                                    { $1 }
        |   ApplExp
                                    { $1 }


ApplExp :   ApplExp LabelExp
                                    { LA.appl $1 $2 }
        |   LabelExp
                                    { $1 }


LabelExp:   '`' ident LabelExp
                                    { TA.label (labelName $2) Nothing $3 }
        |   '`' ident Modifier LabelExp
                                    { TA.label (labelName $2) (Just $3) $4 }
        |   Primary
                                    { $1 }


Primary :   ident
                                    { TA.var (ident $1) }
        |   intLit
                                    { TA.primInt $1 }
        |   charLit
                                    { TA.primChar $1 }
        |   '(' ')'
                                    { TA.primUnit }
        |   '(' '&' ')'
                                    { TA.emptyOnion }
        |   '(' Exp ')'
                                    { $2 }
        |   Primary '.' ident
                                    { LA.proj $1 (ident $3) }
        |   self
                                    { LA.self }
        |   prior
                                    { LA.prior }

Pattern :   ident
                                    { TA.Pattern (ident $1) $ TA.PatOnion [] }
        |   PatternPrimary
                                    { TA.Pattern (ident "_") $1 }
        |   ident ':' PatternPrimary
                                    { TA.Pattern (ident $1) $3 }

PatternPrimary
        :   PrimitiveType
                                    { TA.PatPrim $1 }
        |   '`' ident ident
                                    { TA.PatLabel (labelName $2) (ident $3) $
                                        TA.PatOnion [] }
        |   '`' ident ident ':' PatternPrimary
                                    { TA.PatLabel (labelName $2) (ident $3) $5 }
        |   any
                                    { TA.PatOnion [] }
        |   '(' PatternPrimaryOnionList ')'
                                    { TA.PatOnion $2 }
        |   fun
                                    { TA.PatFun }

PatternPrimaryOnionList
        :   PatternPrimary
                                    { [$1] }
        |   PatternPrimary '&' PatternPrimaryOnionList
                                    { $1:$3 }

PrimitiveType
        :   int                     { PrimInt }
        |   char                    { PrimChar }
        |   unit                    { PrimUnit }

Modifier
        :   final                   { TA.Final }
        |   immut                   { TA.Immutable }

ProjTerm
        :   int                     { ProjPrim PrimInt }
        |   char                    { ProjPrim PrimChar }
        |   unit                    { ProjPrim PrimUnit }
        |   '`' ident               { ProjLabel (labelName $2) }
        |   fun                     { ProjFunc }

OpExp   :   Primary Op Primary      { $2 $1 $3 }

Op      :   '+'                     { TA.lazyOp TA.Plus }
        |   '-'                     { TA.lazyOp TA.Minus }
        |   '=='                    { TA.eagerOp TA.Equal }
        |   '<='                    { TA.eagerOp TA.LessEqual }
        |   '>='                    { TA.eagerOp TA.GreaterEqual }

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

parseLittleBang :: [L.Token] -> Either ParseError LA.Expr
parseLittleBang tokens =
    runIdentity $ runErrorT $ doParseLittleBang tokens
}
