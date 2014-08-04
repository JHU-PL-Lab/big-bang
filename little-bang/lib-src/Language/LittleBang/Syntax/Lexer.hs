module Language.LittleBang.Syntax.Lexer
( lexLittleBang
, Token(..)
, PositionalToken(..)
) where

import Control.Applicative((<$>), (<*>), (<*), (*>), pure)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Parsec)

import Language.LittleBang.Syntax.Tokens
import Language.TinyBang.Syntax.Location

-- |The Lexer type.
type Lexer a = Parsec String () a

-- | A function to lex a @String@ containing LittleBang code.  On error, the
--   result is a left error string; otherwise, it is a right positional token
--   list.
lexLittleBang :: SourceDocument -> String -> Either String [PositionalToken]
lexLittleBang resourceDoc src =
  case parse lexer (nameOfDocument resourceDoc) src of
    Left x -> Left $ show x
    Right x -> Right x

-- | A lexer which obtains all tokens in a given stream
lexer :: Lexer [PositionalToken]
lexer = nonTokens *> many (posTokenLexer <* nonTokens) <* eof

-- | A lexer which matches non-token content.  This includes whitespace and
--   comments.
nonTokens :: Lexer ()
nonTokens = many (choice [comment, space *> return ()]) *> return ()

-- | A lexer which matches LittleBang comments.  Any # character followed by a
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
    , ("==", TokEq)
    , ("()", TokEmptyOnion)
    , ("+" , TokPlus)
    , ("-" , TokMinus)
    , ("<=", TokLessEq)    
    , (">=", TokGreaterEq) 
    , ("<-", TokLeftArrow) 
    , ("&" , TokOnion)
    , ("=" , TokIs)
    , ("(" , TokOpenParen)
    , (")" , TokCloseParen)
    , (";" , TokSemi)
    , ("::", TokCons)
    , (":" , TokColon)
    , ("[" , TokOpenBracket)
    , ("]" , TokCloseBracket)
    , ("," , TokComma)
    , ("...", TokEllipse)
    , ("\\", TokBackslash)
    , ("." , TokDot)
    , ("]", TokCloseBracket)
    , (",", TokComma)
    , ("!", TokDeref)
    ]

-- | This group of lexers matches all reserved words.
reservedWords :: [Parser Token]
reservedWords =
  map (\(s, t) -> string s *> notFollowedBy identChar *> pure t)
    [ ("fun"   , TokFun)
    , ("int"   , TokInt)
    , ("let"   , TokLet)
    , ("in"    , TokIn)
    , ("if"    , TokIf)
    , ("then"  , TokThen)
    , ("else"  , TokElse)
    , ("ref"   , TokRef)
    , ("object", TokObject)
    ]

-- | This group of lexers matches parametric tokens.
variableLengthLexers :: [Parser Token]
variableLengthLexers =
  [ identifierLexer
  , labelLexer
  , integerLexer
  , characterLexer
  ]
  where
    identifierLexer = TokIdentifier .: (:) <$> identStartChar <*> many identChar
    labelLexer = TokLabel .: (:) <$>
      (char '`' *> identStartChar) <*> many identChar
    integerLexer = TokLitInt . read .: (:) <$>
      option ' ' (char '-') <*> many1 digit <* notFollowedBy identChar
    characterLexer = TokLitChar <$> between (char '\'') (char '\'') anyChar
    -- |A convenient operator for composing a unary operator with a binary one.
    (.:) = (.) . (.)
    infixr 8 .:

