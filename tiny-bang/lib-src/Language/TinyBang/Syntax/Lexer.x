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

$digit = 0-9                 -- digits
$alpha = [a-zA-Z]            -- alphabetic characters
$character = [\x00-\x10ffff] -- characters
$identstart = $alpha
$identcont = [$alpha $digit \_ \']

tokens :-

  $white+                      ;
  "#".*                        ;
  int                          { simply TokInt }
  char                         { simply TokChar }
  ref                          { simply TokRef }
  getChar                      { simply TokGetChar }
  putChar                      { simply TokPutChar }
  "->"                         { simply TokArrow }
  "()"                         { simply TokEmptyOnion }
  "=="                         { simply TokEq }
  "<="                         { simply TokLessEq }
  ">="                         { simply TokGreaterEq }
  "<-"                         { simply TokSet }
  "&"                          { simply TokOnion }
  "="                          { simply TokIs }
  ";"                          { simply TokSemi }
  "\"                          { simply TokBackslash }
  "{"                          { simply TokStartBlock }
  "}"                          { simply TokStopBlock }
  "+"                          { simply TokPlus }
  "*"                          { simply TokAsterisk }                         
  "/"                          { simply TokDiv }
  "%"                          { simply TokMod }
  "-"?$digit+                  { wrapM $ \s ->
                                   case readMaybe s of
                                     Just i ->
                                        return $ \ss -> S.token TokLitInt ss i
                                     Nothing ->
                                       alexError $
                                         "Invalid integer literal: " ++ s
                               }
  "-"                          { simply TokMinus }
  "'" [\\]? $character "'"     { wrapM $ \s ->
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
  alexyGetInput = alexGetInput
  alexyInputPosnStr (p,_,_,s) = (p,s)
  alexyPosnLineCol (AlexPn _ x y) = (x,y)
  alexyMonadScan = alexMonadScan
  runAlexy = runAlex
  alexyEofTokenType = return TokEOF

lexTinyBang :: SourceDocument -> String -> Either String [Token]
lexTinyBang = lexTokens (Proxy :: Proxy Alex) 

}
