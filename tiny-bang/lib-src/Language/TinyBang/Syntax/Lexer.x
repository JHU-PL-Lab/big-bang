{
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Language.TinyBang.Syntax.Lexer
( lexTinyBang
)
where

import Data.Proxy

import Language.TinyBang.Syntax.Tokens
import Language.TinyBang.Utils.Syntax as S
import Utils.Monad.Read
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$identstart = $alpha
$identcont = [$alpha $digit \_ \']

tokens :-

  $white+                      ;
  "#".*                        ;
  int                          { simply TokInt }
  ref                          { simply TokRef }
  "->"                         { simply TokArrow }
  "()"                         { simply TokEmptyOnion }
  "=="                         { simply TokEq }
  "<="                         { simply TokLessEq }
  ">="                         { simply TokGreaterEq }
  "<-"                         { simply TokSet }
  "&"                          { simply TokOnion }
  "="                          { simply TokIs }
  ";"                          { simply TokSemi }
  "{"                          { simply TokStartBlock }
  "}"                          { simply TokStopBlock }
  "+"                          { simply TokPlus }
  "-"?$digit+                  { wrapM $ \s ->
                                   case readMaybe s of
                                     Just i ->
                                        return $ \ss -> S.token TokLitInt ss i
                                     Nothing ->
                                       alexError $
                                         "Invalid integer literal: " ++ s
                               }
  "-"                          { simply TokMinus }
  $identstart $identcont*      { wrap $ \s ss -> S.token TokIdentifier ss s }
  `$identcont*                 { wrap $ \s ss ->
                                    S.token TokLabel ss $ drop 1 s
                               }

{

alexEOF :: Alex (PosAlexReturnType TokenType)
alexEOF = genAlexEOF

instance Alexy Alex AlexInput AlexPosn TokenType where
  alexInputPosnStr (p,_,_,s) = (p,s)
  alexPosnLineCol (AlexPn _ x y) = (x,y)
  alexMonadDoScan = alexMonadScan
  runAlexMonad = runAlex

lexTinyBang :: SourceDocument -> String -> Either String [Token]
lexTinyBang = lexTokens (Proxy :: Proxy Alex) 

}