module Language.TinyBang.Syntax.Lexer
( lexTinyBang
, LexerErr
) where

import Control.Applicative((<$), (<$>), (<*>), (<*), (*>), pure)
import Data.Functor.Identity
import Text.Parsec

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Tokens

-- |The Lexer type.
type Lexer a = ParsecT String () Identity a

-- |The error type for this lexer.
type LexerErr = String

-- | A function to lex a @String@ containing TinyBang code.  On error, the
--   result is a left error string; otherwise, it is a right positional token
--   list.
lexTinyBang :: SourceDocument -> String -> Either LexerErr [PositionalToken]
lexTinyBang doc src =
  case runIdentity $ runParserT lexer () (nameOfDocument doc) src of
    Left x -> Left $ show x
    Right x -> Right x

-- | A lexer which obtains all tokens in a given stream
lexer :: Lexer [PositionalToken]
lexer = nonTokens *> many (posTokenLexer <* nonTokens) <* eof

-- | A lexer which matches non-token content.  This includes whitespace and
--   comments.
nonTokens :: Lexer ()
nonTokens = many (choice [comment, space *> return ()]) *> return ()

-- | A lexer which matches TinyBang comments.  Any # character followed by a
--   space or alphanumeric character is a comment until the end of the current
--   line.
comment :: Lexer ()
comment = try $
  char '#' *> choice [space, alphaNum] *> manyTill anyChar newline *> return ()

-- | A lexer which obtains a single positional token from a stream
posTokenLexer :: Lexer PositionalToken
posTokenLexer = do
  (start,tok,stop) <- (,,) <$> getPosition <*> tokenLexer <*> getPosition
  let startPosition = DocumentPosition (sourceLine start) (sourceColumn start)
  let stopPosition = DocumentPosition (sourceLine stop) (sourceColumn stop)
  return $ PositionalToken startPosition stopPosition tok
  
-- | A lexer which obtains a single token from a stream
tokenLexer :: Lexer Token
tokenLexer = choice $ map try $
  concat [reservedWords, operators, variableLengthLexers]

-- | A lexer which matches a character which may appear within an identifier.
identChar :: Lexer Char
identChar = alphaNum <|> char '_'

-- | A lexer which matches a character which may start an identifier.
identStartChar :: Lexer Char
identStartChar = letter <|> char '_'

-- | This group of lexers matches all operators and similar symbols.
operators :: [Lexer Token]
operators = map (\(s,t) -> string s *> pure t)
    [ ("->", TokArrow)
    , ("()", TokEmptyOnion)
    , ("&", TokOnion)
    , ("=", TokIs)
    , ("(", TokOpenParen)
    , (")", TokCloseParen)
    , (";", TokSemi)
    , ("{", TokStartBlock)
    , ("}", TokStopBlock)
    ]

-- | This group of lexers matches all reserved words.
reservedWords :: [Lexer Token]
reservedWords =
  map (\(s, t) -> string s *> notFollowedBy identChar *> pure t)
    [ ("int"   , TokInt)
    ]

-- | This group of lexers matches parametric tokens.
variableLengthLexers :: [Lexer Token]
variableLengthLexers =
  [ identifierLexer
  , labelLexer
  , integerLexer
  ]
  where
    identifierLexer = TokIdentifier .: (:) <$> identStartChar <*> many identChar
    labelLexer = TokLabel <$ char '`' <*> many1 identChar
    integerLexer = TokLitInt . read .: (:) <$>
      option ' ' (char '-') <*> many1 digit <* notFollowedBy identChar
    -- |A convenient operator for composing a unary operator with a binary one.
    (.:) = (.) . (.)
    infixr 8 .:
