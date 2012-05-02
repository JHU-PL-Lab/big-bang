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
import Utils.Language.Ast
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

Exp     :   '\\' ident '->' Exp
                                    { astwrap $ LA.Func (ident $2) $4 }
        |   fun ident '->' Exp
                                    { astwrap $ LA.Func (ident $2) $4 }
        |   def ident '=' Exp in Exp
                                    { astwrap $
                                        TA.Def Nothing (ident $2) $4 $6 }
        |   def Modifier ident '=' Exp in Exp
                                    { astwrap $
                                        TA.Def (Just $2) (ident $3) $5 $7 }
        |   ident '=' Exp in Exp
                                    { astwrap $
                                        TA.Assign (TA.AIdent $ ident $1) $3 $5 }
        |   Primary '.' ident '=' Exp in Exp
                                    { astwrap $
                                        LA.ProjAssign $1 (ident $3) $5 $7 }
        |   case Exp of '{' Branches '}'
                                    { astwrap $ LA.Case $2 $5 }
        |   Exp '&' Exp
                                    { astwrap $ LA.Onion $1 $3 }
        |   Exp '&-' ProjTerm
                                    { astwrap $ TA.OnionSub $1 $3 }
        |   Exp '&.' ProjTerm
                                    { astwrap $ TA.OnionProj $1 $3 }
        |   OpExp
                                    { $1 }
        |   ApplExp
                                    { $1 }


ApplExp :   ApplExp LabelExp
                                    { astwrap $ LA.Appl $1 $2 }
        |   LabelExp
                                    { $1 }


LabelExp:   '`' ident LabelExp
                                    { astwrap $
                                        TA.Label (labelName $2) Nothing $3 }
        |   '`' ident Modifier LabelExp
                                    { astwrap $
                                        TA.Label (labelName $2) (Just $3) $4 }
        |   Primary
                                    { $1 }


Primary :   ident
                                    { astwrap $ TA.Var (ident $1) }
        |   intLit
                                    { astwrap $ TA.PrimInt $1 }
        |   charLit
                                    { astwrap $ TA.PrimChar $1 }
        |   '(' ')'
                                    { astwrap $ TA.PrimUnit }
        |   '(' '&' ')'
                                    { astwrap $ TA.EmptyOnion }
        |   '(' Exp ')'
                                    { $2 }
        |   Primary '.' ident
                                    { astwrap $ LA.Proj $1 (ident $3) }
        |   self
                                    { astwrap $ LA.Self }
        |   prior
                                    { astwrap $ LA.Prior }


Branches:   Branch ';' Branches     { $1:$3 }
        |   Branch                  { [$1] }


Branch  :   Pattern '->' Exp        { TA.Branch $1 $3 }


Pattern :   ident                  
                                    { TA.ChiTopVar $ ident $1 }
        |   PatternPrimary '&' PatternStruct
                                    { TA.ChiTopOnion $1 $3 }
        |   PatternBind
                                    { TA.ChiTopBind $1 }

PatternStruct
        :   PatternPrimary '&' PatternStruct
                                    { TA.ChiOnionMany $1 $3 }
        |   PatternPrimary
                                    { TA.ChiOnionOne $1 }

PatternBind
        :   ident ':' PatternBind   { TA.ChiBound (ident $1) $3 }
        |   PatternPrimary          { TA.ChiUnbound $1 }

PatternPrimary
        :   PrimitiveType           { TA.ChiPrim $1 }
        |   '`' ident ident         { TA.ChiLabelShallow (labelName $2) $ ident $3 }
        |   '`' ident PatternBind   { TA.ChiLabelDeep (labelName $2) $3 }
        |   fun                     { TA.ChiFun }
        |   '(' PatternStruct ')'   { TA.ChiInnerStruct $2 }

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

OpExp   :   Primary Op Primary      { astwrap $ $2 $1 $3 }

Op      :   '+'                     { TA.LazyOp TA.Plus }
        |   '-'                     { TA.LazyOp TA.Minus }
        |   '=='                    { TA.EagerOp TA.Equal }
        |   '<='                    { TA.EagerOp TA.LessEqual }
        |   '>='                    { TA.EagerOp TA.GreaterEqual }

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
