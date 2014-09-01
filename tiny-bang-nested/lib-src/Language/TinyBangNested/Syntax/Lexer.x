{
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Language.TinyBangNested.Syntax.Lexer
( lexTinyBangNested
) where

import Data.Proxy

import Language.TinyBang.Utils.Syntax as S
import Language.TinyBangNested.Syntax.Tokens
import Utils.Monad.Read
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$character = [\x00-\x10ffff]
$identstart = $alpha
$identcont = [$alpha $digit \_ \']

tokens :-

  $white+                      ;
  "#".*                        ;
  int                          { simply TokInt }
  char                         { simply TokChar }
  ref                          { simply TokRef }
  let                          { simply TokLet }
  in                           { simply TokIn }
  "fun"                        { simply TokLambda }
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
  "("                          { simply TokOpenParen }
  ")"                          { simply TokCloseParen }
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
  "'" "\\"? $character* "'"    { wrapM $ \s ->
                                   case readMaybe s of
                                     Just i ->
                                        return $ \ss -> S.token TokLitChar ss i
                                     Nothing ->
                                       alexError $
                                         "Invalid character literal: " ++ s
                               }
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

lexTinyBangNested :: SourceDocument -> String -> Either String [Token]
lexTinyBangNested = lexTokens (Proxy :: Proxy Alex) 

}
