{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}

{-|
  This module contains utilities to simplify the process of creating an Alex
  lexer which produces tokens of the style given in
  @Language.TinyBang.Utils.Syntax.Tokens@.  In particular, these utilities
  handle the threading of source span information through the token by providing
  them as input to lexer actions through wrapper functions.  A valid rule in a
  monadic Alex parser, for instance, may be
  @
      `$identcont* { wrap $ \s ss -> T.token TokLabel ss $ drop 1 s } 
  @
  where the @wrap@ function is provided by this module.
  
  Because each Alex lexer defines its own types, these library functions operate
  through a typeclass, @Alexy@, defined in this module.  An instance of that
  typeclass must be given within the lexer module.  Functions which are required
  by Alex (e.g. @alexEOF@) must also be defined within the lexer file.  An
  example of such a snippet follows.
  @
    alexEOF :: Alex (PosAlexReturnType TokenType)
    alexEOF = genAlexEOF
    
    instance Alexy Alex AlexInput AlexPosn TokenType where
      alexInputPosnStr (p,_,_,s) = (p,s)
      alexPosnLineCol (AlexPn _ x y) = (x,y)
      alexMonadDoScan = alexMonadScan
      runAlexMonad = runAlex
    
    lexTinyBang :: SourceDocument -> String -> Either String [Token]
    lexTinyBang = lexTokens (Proxy :: Proxy Alex) 
  @
-}
module Language.TinyBang.Utils.Syntax.Lexer
( Alexy(..)
, wrapM
, wrap
, simply
, genAlexEOF
, lexTokens
, PosAlexReturnType
, PosAlexTokenType
) where

import Control.Applicative
import Control.Monad.Reader
import Data.Proxy

import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.Utils.Syntax.Tokens as T

-- |A typeclass which defines the interface necessary to communicate with an
--  Alex lexer.  The type parameters specify Alex-generated types; the methods
--  give operations that this library needs over those types.  The type
--  variables themselves are the (lower-cased) names of the Alex types.
class (Monad alex) => Alexy alex alexInput alexPosn tokenType
          | alex -> alexInput alexPosn tokenType
          , alexInput -> alex alexPosn tokenType
          , alexPosn -> alex alexInput tokenType where
  alexInputPosnStr :: alexInput -> (alexPosn, String)
  alexPosnLineCol :: alexPosn -> (Int,Int)
  runAlexMonad :: String -> alex a -> Either String a
  alexMonadDoScan :: alex (PosAlexReturnType tokenType)

-- |A utility to create positional tokens for Alex.  The first argument of this
--  function should be a function which accepts a string from Alex and yields an
--  appropriate token.  This function will then yield a two-argument function
--  suitable as an Alex monad wrapper action.
wrapM :: Alexy alex alexInput alexPosn tokenType
      => (String -> alex (SourceSpan -> PosAlexTokenType tokenType))
      -> alexInput
      -> Int
      -> alex (PosAlexReturnType tokenType)
wrapM f inp len = do
  let (posn, str) = alexInputPosnStr inp
  let (lineNum, colNum) = alexPosnLineCol posn
  g <- f (take len str)
  let start = DocumentPosition lineNum colNum
  let stop = DocumentPosition lineNum (colNum + len - 1)
  let docSpanR = DocumentSpan <$> ask <*> pure start <*> pure stop
  let tokenR = g <$> docSpanR
  return $ Just <$> tokenR

-- |A utility to create positional tokens for Alex.  The first argument of this
--  function should be a function which accepts a string from Alex and yields an
--  appropriate token.  This function will then yield a two-argument function
--  suitable as an Alex monad wrapper action.
wrap :: Alexy alex alexInput alexPosn tokenType
     => (String -> SourceSpan -> PosAlexTokenType tokenType)
     -> alexInput
     -> Int
     -> alex (PosAlexReturnType tokenType)
wrap f =
  let g s = return $ \ss -> f s ss in
  wrapM g

-- |A utility to create positional tokens for Alex.  The first argument to this
--  function should be a Token value; it is used regardless of the source text
--  (and is meant for tokens which are defined exclusively interms of the
--  particular matcher used to identify it).
simply :: Alexy alex alexInput alexPosn tokenType
       => tokenType () -> alexInput -> Int -> alex (PosAlexReturnType tokenType)
simply tt = wrap $ const $ \ss -> T.token tt ss ()

-- |The error type for this lexer.
type LexerErr = String

-- |The type of token assumed to be used with this lexer.
type PosAlexTokenType t = TypedToken t

-- |The token type for this lexer.
type PosAlexReturnType t = Reader SourceDocument (Maybe (TypedToken t))

genAlexEOF :: Alexy alex alexInput alexPosn tokenType
        => alex (PosAlexReturnType token)
genAlexEOF = return $ return Nothing

lexTokens :: forall alex alexInput alexPosn tokenType.
             (Monad alex, Alexy alex alexInput alexPosn tokenType)
          => Proxy alex
          -> SourceDocument
          -> String
          -> Either LexerErr [PosAlexTokenType tokenType]
lexTokens _ doc input =
  runAlexMonad input readTokens
  where
    readTokens :: alex [PosAlexTokenType tokenType]
    readTokens = do
      rmtok <- alexMonadDoScan
      let mtok = runReader rmtok doc
      case mtok of
        Nothing -> return []
        Just pt -> (pt:) `liftM` readTokens
