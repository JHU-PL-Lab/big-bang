{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, TupleSections #-}

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
      alexyGetInput = alexGetInput
      alexyInputPosnStr (p,_,_,s) = (p,s)
      alexyPosnLineCol (AlexPn _ x y) = (x,y)
      alexyMonadScan = alexMonadScan
      runAlexy = runAlex
      alexyEofTokenType = return TokEOF
      
    lexMyTokens :: SourceDocument -> String -> Either String [Token]
    lexMyTokens = lexTokens (Proxy :: Proxy Alex) 
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
import Language.TinyBang.Utils.Syntax.Positional
import Language.TinyBang.Utils.Syntax.Tokens as T

-- |A typeclass which defines the interface necessary to communicate with an
--  Alex lexer.  The type parameters specify Alex-generated types; the methods
--  give operations that this library needs over those types.  The type
--  variables themselves are the (lower-cased) names of the Alex types.
class (Monad alex) => Alexy alex alexInput alexPosn tokenType
          | alex -> alexInput alexPosn tokenType
          , alexInput -> alex alexPosn tokenType
          , alexPosn -> alex alexInput tokenType where
  alexyGetInput :: alex alexInput 
  alexyInputPosnStr :: alexInput -> (alexPosn, String)
  alexyPosnLineCol :: alexPosn -> (Int,Int)
  runAlexy :: String -> alex a -> Either String a
  alexyMonadScan :: alex (PosAlexReturnType tokenType)
  alexyEofTokenType :: alex (tokenType ())

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
  let (posn, str) = alexyInputPosnStr inp
  let (lineNum, colNum) = alexyPosnLineCol posn
  g <- f (take len str)
  let start = DocumentPosition lineNum colNum
  let stop = DocumentPosition lineNum (colNum + len - 1)
  let docSpanR = DocumentSpan <$> ask <*> pure start <*> pure stop
  let tokenR = g <$> docSpanR
  return $ (,False) <$> tokenR

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
type PosAlexReturnType t = Reader SourceDocument (PosAlexTokenType t, Bool)

genAlexEOF :: Alexy alex alexInput alexPosn tokenType
        => alex (PosAlexReturnType tokenType)
genAlexEOF = do
  input <- alexyGetInput
  tokenType <- alexyEofTokenType
  let (posn, _) = alexyInputPosnStr input
  let (lineNum, colNum) = alexyPosnLineCol posn
  let pos = DocumentPosition lineNum colNum
  return $ do
    doc <- ask
    let docSpan = DocumentSpan doc pos pos
    return (Token $ SomeToken tokenType $ spos docSpan (), True)

lexTokens :: forall alex alexInput alexPosn tokenType.
             (Monad alex, Alexy alex alexInput alexPosn tokenType)
          => Proxy alex
          -> SourceDocument
          -> String
          -> Either LexerErr [PosAlexTokenType tokenType]
lexTokens _ doc input =
  runAlexy input readTokens
  where
    readTokens :: alex [PosAlexTokenType tokenType]
    readTokens = do
      rptok <- alexyMonadScan
      let (tok,finalToken) = runReader rptok doc
      (tok:) `liftM` (if finalToken then return [] else readTokens)
