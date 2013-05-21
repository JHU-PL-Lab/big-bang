module Language.TinyBang.Syntax.Lexer
( lexTinyBang
, Token(..)
, PositionalToken(..)
, SourceLocation(..)
) where

import Control.Applicative((<$>), (<*>), (<*), (*>), pure)
import Text.ParserCombinators.Parsec

import Language.TinyBang.Display (Display(..), text, dquotes, (<>))
import Language.TinyBang.Syntax.Location
import Text.Parsec.Prim (Parsec)

-- |The raw tokens generated by this lexer.
data Token
  = TokIs -- ^@=@
  | TokGets -- ^@<-@
  | TokThrows -- ^@throws@
  | TokDef -- ^@:=@
  | TokFlows Char -- ^@<~X@
  | TokBang -- ^@!@
  | TokPlus -- ^@+@
  | TokMinus -- ^@-@
  | TokLT -- ^@<@
  | TokGT -- ^@>@
  | TokEq -- ^@==@
  | TokEmptyOnion -- ^@()@
  | TokOnion -- ^@&@
  | TokOnionSub -- ^@&-@
  | TokOnionProj -- ^@&.@
  | TokArrow -- ^@->@
  | TokImmut -- ^@immut@
  | TokFinal -- ^@final@
  | TokExn -- ^@exn@
  | TokFun -- ^@fun@
  | TokInt -- ^@int@
  | TokChar -- ^@char@
  | TokColon -- ^@:@
  | TokOpenParen -- ^@(@
  | TokCloseParen -- ^@)@
  | TokSemi -- ^@;@
  | TokOpenBrace -- ^@{@
  | TokCloseBrace -- ^@}@
  | TokIdentifier String
  | TokLitInt Integer
  | TokLitChar Char
  | TokLabel String -- ^The @String@ is only the name of the label, not the @`@
  deriving (Eq, Ord, Show)

-- |The characters which are accepted as flow kinds
flowKinds :: String
flowKinds = "X"
  
-- |An annotation for tokens which describes their Parsec source position.
data PositionalToken
  = PositionalToken { startPos :: SourcePos
                    , stopPos :: SourcePos
                    , posToken :: Token }
  deriving (Eq, Ord, Show)
  
-- |The Lexer type.
type Lexer a = Parsec String () a

-- | A function to lex a @String@ containing TinyBang code.  On error, the
--   result is a left error string; otherwise, it is a right positional token
--   list.
lexTinyBang :: String -> String -> Either String [PositionalToken]
lexTinyBang resourceName src =
  case parse lexer resourceName src of
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
  return $ PositionalToken start stop tok
  
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
    [ ("&-", TokOnionSub)
    , ("&.", TokOnionProj)
    , ("->", TokArrow)
    , ("<-", TokGets)
    , (":=", TokDef)
    , ("==", TokEq)
    , ("()", TokEmptyOnion)
    , ("!", TokBang)
    , ("+", TokPlus)
    , ("-", TokMinus)
    , ("<", TokLT)
    , (">", TokGT)
    , ("&", TokOnion)
    , (":", TokColon)
    , ("=", TokIs)
    , ("(", TokOpenParen)
    , (")", TokCloseParen)
    , (";", TokSemi)
    , ("{", TokOpenBrace)
    , ("}", TokCloseBrace)
    ]
    ++
    [
      TokFlows <$> (string "<~" *> oneOf flowKinds) 
    ]

-- | This group of lexers matches all reserved words.
reservedWords :: [Parser Token]
reservedWords =
  map (\(s, t) -> string s *> notFollowedBy identChar *> pure t)
    [ ("throws", TokThrows)
    , ("immut" , TokImmut)
    , ("final" , TokFinal)
    , ("exn"   , TokExn)
    , ("fun"   , TokFun)
    , ("int"   , TokInt)
    , ("char"  , TokChar)
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

instance Display Token where
  makeDoc t = case t of
    TokIs -> dquotes $ text "="
    TokGets -> dquotes $ text "<-"
    TokThrows -> dquotes $ text "throws"
    TokDef -> dquotes $ text ":="
    TokFlows k -> dquotes $ text $ "<~" ++ [k]
    TokBang -> dquotes $ text "!"
    TokPlus -> dquotes $ text "+"
    TokMinus -> dquotes $ text "-"
    TokLT -> dquotes $ text "<"
    TokGT -> dquotes $ text ">"
    TokEq -> dquotes $ text "=="
    TokEmptyOnion -> dquotes $ text "()"
    TokOnion -> dquotes $ text "&"
    TokOnionSub -> dquotes $ text "&-"
    TokOnionProj -> dquotes $ text "&."
    TokArrow -> dquotes $ text "->"
    TokImmut -> dquotes $ text "immut"
    TokFinal -> dquotes $ text "final"
    TokExn -> dquotes $ text "exn"
    TokFun -> dquotes $ text "fun"
    TokInt -> dquotes $ text "int"
    TokChar -> dquotes $ text "char"
    TokColon -> dquotes $ text ":"
    TokOpenParen -> dquotes $ text "("
    TokCloseParen -> dquotes $ text ")"
    TokSemi -> dquotes $ text ";"
    TokOpenBrace -> dquotes $ text "{"
    TokCloseBrace -> dquotes $ text "}"
    TokIdentifier s -> text "id#" <> dquotes (text s)
    TokLitInt n -> text "int#" <> dquotes (text $ show n)
    TokLitChar c -> text "char#" <> dquotes (text [c])
    TokLabel n -> text "label#" <> dquotes (text n)
