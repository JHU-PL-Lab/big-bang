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
import Language.TinyBang.Utils.Syntax.Tokens as T
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
  "-"?$digit+                  { wrap $ \s ss -> T.token TokLitInt ss $ read s } -- TODO: fix to use Alex errors
  "-"                          { simply TokMinus }
  $identstart $identcont*      { wrap $ \s ss -> T.token TokIdentifier ss s }
  `$identcont*                 { wrap $ \s ss -> T.token TokLabel ss $ drop 1 s } 

{
-- |A utility to create positional tokens for Alex.  The first argument of this
--  function should be a function which accepts a string from Alex and yields an
--  appropriate token.  This function will then yield a two-argument function
--  suitable as an Alex monad wrapper action.
wrap :: (String -> SourceSpan -> Token)
     -> AlexInput
     -> Int
     -> Alex AlexTokenType
wrap f inp len = do
  let (posn, _, _, str) = inp
  let (AlexPn _ lineNum colNum) = posn
  let start = DocumentPosition lineNum colNum
  let stop = DocumentPosition lineNum (colNum + len - 1)
  let g :: String -> Reader SourceDocument Token
      g s = do
              doc <- ask
              let docspan = DocumentSpan doc start stop
              return $ f s docspan
  let tokR = g (take len str)
  return $ do
    doc <- ask
    let tok = runReader tokR doc
    return $ Just tok

-- |A utility to create positional tokens for Alex.  The first argument to this
--  function should be a Token value; it is used regardless of the source text
--  (and is meant for tokens which are defined exclusively interms of the
--  particular matcher used to identify it).
simply :: TokenType () -> AlexInput -> Int -> Alex AlexTokenType
simply tt = wrap $ (const $ \ss -> T.token tt ss ())

-- |The error type for this lexer.
type LexerErr = String

-- |The token type for this lexer.
type AlexTokenType = Reader SourceDocument (Maybe Token)

alexEOF :: Alex AlexTokenType
alexEOF = return $ return Nothing

lexTinyBang :: SourceDocument -> String -> Either LexerErr [Token]
lexTinyBang doc input =
  runAlex input readTokens
  where
    readTokens :: Alex [Token]
    readTokens = do
      rmtok <- alexMonadScan
      let mtok = runReader rmtok doc
      case mtok of
        Nothing -> return []
        Just pt -> (pt:) `liftM` readTokens 

}