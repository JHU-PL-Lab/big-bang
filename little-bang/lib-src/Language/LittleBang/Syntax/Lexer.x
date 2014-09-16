{
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Language.LittleBang.Syntax.Lexer
( lexLittleBang
)
where

import Data.Proxy

import Language.LittleBang.Syntax.Tokens
import Language.TinyBang.Utils.Syntax as S
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
  let                          { simply TokLet }
  in                           { simply TokIn }
  fun                          { simply TokLambda }
  if                           { simply TokIf }
  then                         { simply TokThen }
  else                         { simply TokElse }
  int                          { simply TokInt }
  char                         { simply TokChar }
  ref                          { simply TokRef }
  object                       { simply TokObject }
  class                        { simply TokClass }
  getChar                      { simply TokGetChar }
  putChar                      { simply TokPutChar }
  "->"                         { simply TokArrow }
  "()"                         { simply TokEmptyOnion }
  "=="                         { simply TokEq }
  "<="                         { simply TokLessEq }
  ">="                         { simply TokGreaterEq }
  "<"                          { simply TokLess }
  "<-"                         { simply TokSet }
  "&"                          { simply TokOnion }
  "="                          { simply TokIs }
  "("                          { simply TokOpenParen }
  ")"                          { simply TokCloseParen }
  "["                          { simply TokOpenBracket }
  "]"                          { simply TokCloseBracket }
  "{"                          { simply TokOpenBrace }
  "}"                          { simply TokCloseBrace }
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
  "'" "\\"? $character* "'"     { wrapM $ \s ->
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
  ","                          { simply TokComma }
  "::"                         { simply TokCons }
  ":"                          { simply TokColon }
  ";"                          { simply TokSemi }
  "!"                          { simply TokDeref }
  "."                          { simply TokDot }
  "~"                          { simply TokTilde }

{

alexEOF :: Alex (PosAlexReturnType TokenType)
alexEOF = genAlexEOF

instance Alexy Alex AlexInput AlexPosn TokenType where
  alexInputPosnStr (p,_,_,s) = (p,s)
  alexPosnLineCol (AlexPn _ x y) = (x,y)
  alexMonadDoScan = alexMonadScan
  runAlexMonad = runAlex

lexLittleBang :: SourceDocument -> String -> Either String [Token]
lexLittleBang = lexTokens (Proxy :: Proxy Alex) 

}
