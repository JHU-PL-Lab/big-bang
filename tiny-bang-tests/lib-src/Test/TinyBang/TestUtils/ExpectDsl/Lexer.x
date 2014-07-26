{
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Test.TinyBang.TestUtils.ExpectDsl.Lexer
( lexValueDsl
)
where

import Control.Monad

import Test.TinyBang.TestUtils.ExpectDsl.Tokens
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
  "()"                         { simply TokEmptyOnion }
  "&"                          { simply TokOnion }
  "("                          { simply TokOpenParen }
  ")"                          { simply TokCloseParen }
  "-"?$digit+                  { wrapM $ \s ->
                                   case readMaybe s of
                                     Just i -> return $ TokLitInt i
                                     Nothing ->
                                       alexError $
                                         "Invalid integer literal: " ++ s
                               }
  `$identcont*                 { wrapM $ \s -> return $ TokLabel $ drop 1 s }
  ref                          { simply TokRef }
  scapes                       { simply TokScapes }
  typefail                     { simply TokTypeFail }

{

simply :: a -> AlexInput -> Int -> Alex a
simply x = const $ const $ return x

wrapM :: (String -> Alex a) -> AlexInput -> Int -> Alex a
wrapM f (_,_,_,s) n = f $ take n s

alexEOF :: Alex Token
alexEOF = return TokEOF

lexValueDsl :: String -> Either String [Token]
lexValueDsl input =
  runAlex input readTokens
  where
    readTokens :: Alex [Token]
    readTokens = do
      tok <- alexMonadScan
      case tok of
        TokEOF -> return [TokEOF]
        t -> (t:) `liftM` readTokens

}