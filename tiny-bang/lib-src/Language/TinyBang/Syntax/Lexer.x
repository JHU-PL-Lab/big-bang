{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.TinyBang.Syntax.Lexer
( lexTinyBang
)
where

import Control.Monad
import Control.Monad.Reader

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Tokens
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$identstart = $alpha
$identcont = [$alpha $digit \_ \']

tokens :-

  $white+                          ;
  "#".*                            ;
  int                              { simply TokInt }
  ref                              { simply TokRef }
  "->"                             { simply TokArrow }
  "()"                             { simply TokEmptyOnion }
  "=="                             { simply TokEq }
  "<="                             { simply TokLessEq }
  ">="                             { simply TokGreaterEq }
  "<-"                             { simply TokSet }
  "&"                              { simply TokOnion }
  "="                              { simply TokIs }
  "("                              { simply TokOpenParen }
  ")"                              { simply TokCloseParen }
  ";"                              { simply TokSemi }
  "{"                              { simply TokStartBlock }
  "}"                              { simply TokStopBlock }
  "+"                              { simply TokPlus }
  "-"                              { simply TokMinus }
  $identstart $identcont*          { wrap $ \s -> return $ TokIdentifier s }
  `$identcont*                     { wrap $ \s -> return $ TokLabel $ drop 1 s } 
  $digit+                          { wrap $ \s -> return $ TokLitInt (read s) } -- TODO: fix to use Alex errors

{
-- Each right-hand side has type :: String -> Token

-- |A utility to create positional tokens for Alex.  The first argument of this
--  function should be a function which accepts a string from Alex and yields an
--  appropriate token.  This function will then yield a two-argument function
--  suitable as an Alex monad wrapper action.
wrap :: (String -> Alex Token) -> AlexInput -> Int -> Alex AlexTokenType
wrap f inp len = do
  let (posn, _, _, str) = inp
  tok <- f (take len str)
  let (AlexPn _ lineNum colNum) = posn
  let start = DocumentPosition lineNum colNum
  let stop = DocumentPosition lineNum (colNum + len - 1)
  return $ do
             doc <- ask
             let docspan = DocumentSpan doc start stop
             return $ Just $ PositionalToken docspan tok

-- |A utility to create positional tokens for Alex.  The first argument to this
--  function should be a Token value; it is used regardless of the source text
--  (and is meant for tokens which are defined exclusively interms of the
--  particular matcher used to identify it).
simply :: Token -> AlexInput -> Int -> Alex AlexTokenType
simply t = wrap $ const $ return t

-- |The error type for this lexer.
type LexerErr = String

-- |The token type for this lexer.
type AlexTokenType = Reader SourceDocument (Maybe PositionalToken)

alexEOF :: Alex AlexTokenType
alexEOF = return $ return Nothing

lexTinyBang :: SourceDocument -> String -> Either LexerErr [PositionalToken]
lexTinyBang doc input =
  runAlex input readTokens
  where
    readTokens :: Alex [PositionalToken]
    readTokens = do
      rmtok <- alexMonadScan
      let mtok = runReader rmtok doc
      case mtok of
        Nothing -> return []
        Just pt -> (pt:) `liftM` readTokens 

}