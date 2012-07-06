{-# LANGUAGE  ImplicitParams
            , FlexibleContexts
            #-}

module Language.TinyBang.Syntax.Parser
( parseTinyBang
, ParseError
)
where

-- imports from the rest of TinyBang
import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Ast (Pattern(..), PrimaryPattern(..), ProjTerm(..))
import qualified Language.TinyBang.Syntax.Lexer as L
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
    ( ident
    , labelName
    , LabelName
    )
import Data.ExtensibleVariant
import Utils.Render.Display

-- imports for Parsec
import Text.Parsec ( token
                   , many
                   , (<|>)
                   , try
                   , optionMaybe
                   , eof
                   , option
                   , parse
                   , ParseError
                   , Parsec
                   , choice
                   , sepBy1
                   , between
                   , many1
                   , (<?>)
                   , chainl1
                   , notFollowedBy)

import Text.Parsec.Expr

import Control.Applicative ((<$>), (<*>), (*>), (<*), pure, Applicative)

import Debug.Trace (traceShow)

tokIdent   :: L.RawToken
tokIdent   =  L.TokIdentifier $ error "tokIdent's payload examined"
tokCharLit :: L.RawToken
tokCharLit =  L.TokCharLiteral $ error "tokCharLit's payload examined"
tokIntLit  :: L.RawToken
tokIntLit  =  L.TokIntegerLiteral $ error "tokIntLit's payload examined"

type TokParser = Parsec [L.Token] ()

-- TODO: refactor to use Parsec's error messages in some way
parseTinyBang :: [L.Token] -> Either ParseError A.Expr
parseTinyBang ts =
  {-traceShow (map L.getRawToken ts) $-} case parse program "" ts of
    Left x -> Left x
    Right x -> Right x

-- I don't understand how (fst . L.getPos) does what is expected in
-- `token`'s contract, but I'll leave it as is.
isToken :: L.RawToken -> TokParser L.Token
isToken t = token show (fst . L.getPos) isT
    where
        isT t' = if t' `L.weaklyMatchesRawToken` t then Just t' else Nothing

grabIdent :: TokParser String
grabIdent = do
    L.TokIdentifier i <- L.getRawToken <$> isToken tokIdent
    return i

grabInt :: TokParser Integer
grabInt = do
    L.TokIntegerLiteral v <- L.getRawToken <$> isToken tokIntLit
    return v

grabChar :: TokParser Char
grabChar = do
    L.TokCharLiteral v <- L.getRawToken <$> isToken tokCharLit
    return v

--TODO: have a different set of identifiers for labels
lbl :: TokParser LabelName
lbl = isToken L.TokLabelPrefix *> (labelName <$> grabIdent)

qual :: TokParser (Maybe A.Modifier)
qual = choice
  [ isToken L.TokFinal *> rjust A.Final
  , isToken L.TokImmut *> rjust A.Immutable
  , pure Nothing
  ] <?> "qualifier"
  where rjust = pure . Just

tprim :: TokParser T.PrimitiveType
tprim = choice
  [ isToken L.TokInteger *> pure T.PrimInt
  , isToken L.TokChar    *> pure T.PrimChar
  , isToken L.TokUnit    *> pure T.PrimUnit
  ]

projectorTerm :: TokParser A.ProjTerm
projectorTerm = choice
  [ isToken L.TokFun *> pure ProjFunc
  , ProjPrim        <$> tprim
  , ProjLabel       <$> lbl
  ] <?> "projector"
  where rprim :: (Applicative f) => T.PrimitiveType -> f A.ProjTerm
        rprim = pure . ProjPrim

pattern :: TokParser A.Pattern
pattern = choice
  [ A.Pattern (ident "_") <$> primaryPattern
  , A.Pattern <$> (ident <$> grabIdent)
              <*> option (PatOnion []) (isToken L.TokColon *> primaryPattern)
  ]

primaryPattern :: TokParser A.PrimaryPattern
primaryPattern = choice
  [ PatPrim <$> tprim
  , isToken L.TokAny *> pure (PatOnion [])
  , isToken L.TokFun *> pure PatFun
  , toPrimaryPattern <$> lbl <*> pattern
  , onion
  ]
  where toPrimaryPattern :: LabelName -> Pattern -> PrimaryPattern
        toPrimaryPattern l (Pattern i pp) = PatLabel l i pp
        onion = PatOnion <$>
          between (isToken L.TokOpenParen)
                  (isToken L.TokCloseParen)
                  (primaryPattern `sepBy1` isToken L.TokOnionCons)

binwrap :: (A.ExprPart :<< ast)
        => (ast -> ast -> A.ExprPart ast) -> (ast -> ast -> ast)
binwrap f = \x y -> inj $ f x y

op :: TokParser (A.Expr -> A.Expr -> A.Expr)
op = choice
  [ isToken L.TokOpPlus          *> rbin A.LazyOp A.Plus
  , isToken L.TokOpMinus         *> rbin A.LazyOp A.Minus
  , isToken L.TokOpEquals        *> rbin A.EagerOp A.Equal
  , isToken L.TokOpLessEquals    *> rbin A.EagerOp A.LessEqual
  , isToken L.TokOpGreaterEquals *> rbin A.EagerOp A.GreaterEqual
  ] <?> "operator"
  where rbin opType opName = pure $ binwrap $ opType opName

term :: TokParser A.Expr
term = choice
  [ try $ between (isToken L.TokOpenParen)
                  (isToken L.TokCloseParen)
                  expr'
  , try $ inj . A.Var <$> (ident <$> grabIdent)
  , literal
  ]

expr' =
  buildExpressionParser' table term
--                         (try $ term <* notFollowedBy (isToken L.TokArrow))

table =
  [ [ prefix $ unary A.Label labelP]
  , [ binop (pure ()) A.Appl AssocLeft]
  , [ tokBinop L.TokOpPlus  (A.LazyOp A.Plus)  AssocLeft
    , tokBinop L.TokOpMinus (A.LazyOp A.Minus) AssocLeft]
  , [ cmpOp L.TokOpEquals        A.Equal
    , cmpOp L.TokOpLessEquals    A.LessEqual
    , cmpOp L.TokOpGreaterEquals A.GreaterEqual]
  , [ tokBinop L.TokOnionCons A.Onion AssocLeft]
  , [ postfix $ choice [ projector A.OnionSub  L.TokOnionSub
                       , projector A.OnionProj L.TokOnionProj]]
  , [ tokBinop L.TokOnionCons A.Onion AssocLeft]
  , [ prefix $ try $ choice [ unary A.Assign (try . assignP)
                            , unary (A.Def Nothing) defP
                            , unary A.Scape  (try . scapeP)]]
--  , [ prefix scapeParser]
  ]
  where unary con p = p con >>= \f -> return $ inj . f
        -- definitions for prefix and postfix taken from this SO post:
        -- http://stackoverflow.com/questions/10475337
        prefix  p = Prefix  . chainl1 p $ return       (.)
        postfix p = Postfix . chainl1 p $ return (flip (.))
        projector con tok =
          wrapped <$> (isToken tok *> projectorTerm)
          where wrapped proj e = inj $ con e proj
--        postfix1 con p = Postfix $ wrapped <$> p
--          where wrapped a b = inj $ con b a
--        postfix con p = Postfix $ unary con p
        assignP c =
          c <$> (ident <$> grabIdent) <*>
                (isToken L.TokEquals *> expr') <* isToken L.TokIn
        scapeP c = c <$> pattern <* isToken L.TokArrow
        labelP c = c <$> lbl <*> qual
        defP c = isToken L.TokDef *> assignP c
        wrap2 con a1 a2 = inj $ con a1 a2
        binop p con assoc =
          Infix (p *> (pure $ wrap2 con)) assoc
        tokBinop tok con assoc = binop (isToken tok) con assoc
        cmpOp tok con =
          tokBinop tok (A.EagerOp con) AssocNone

scape :: TokParser A.Expr
scape = inj <$> scape'
  where scape' =
          A.Scape <$> pattern <* isToken L.TokArrow <*> expr <?> "scape"


literal :: TokParser A.Expr
literal = inj <$> choice
  [ try unit
  , try emptyOnion
  , A.PrimInt <$> grabInt
  , A.PrimChar <$> grabChar
  ] <?> "literal value"
  where
    unit =
      isToken L.TokOpenParen *>
      isToken L.TokCloseParen *>
      pure A.PrimUnit <?> "unit"
    emptyOnion =
      isToken L.TokOpenParen *>
      isToken L.TokOnionCons *>
      isToken L.TokCloseParen *>
      pure A.EmptyOnion <?> "empty onion"

exprStart :: TokParser A.Expr
exprStart = choice
  [ try literal
  , try scape
  , between (isToken L.TokOpenParen) (isToken L.TokCloseParen) expr
  , inj <$> (A.Label <$> lbl <*> qual <*> expr)
  , var <$> (ident <$> grabIdent) <*> assignmentSuffix
  ]
  where assignmentSuffix =
          optionMaybe ((,) <$> (isToken L.TokEquals *> expr)
                           <*> (isToken L.TokIn     *> expr))
        var i mpair = inj $
          case mpair of
               Nothing -> A.Var i
               Just (e1, e2) -> A.Assign i e1 e2

exprRest :: A.Expr -> TokParser A.Expr
exprRest startOfExpr = choice $ map (fmap inj)
  [ A.Onion     startOfExpr <$> (isToken L.TokOnionCons *> expr)
  , A.OnionSub  startOfExpr <$> (isToken L.TokOnionSub  *> projectorTerm)
  , A.OnionProj startOfExpr <$> (isToken L.TokOnionProj *> projectorTerm)
  , A.Appl      startOfExpr <$> expr
  ] ++
  [ mkOpExpr    startOfExpr <$> op <*> expr
  ]
  where mkOpExpr e1 operator e2 = operator e1 e2

expr :: TokParser A.Expr
expr = do
  e1 <- exprStart
  option e1 (exprRest e1)

program :: TokParser A.Expr
program = expr' <* eof

instance Display ParseError where
    makeDoc err = text (show err)

buildExpressionParser' operators simpleExpr
    = foldl (makeParser) simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix)      = foldr splitOp ([],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              ambigious assoc op= try $
                                  do{ op; fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")
                                    }

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              termP      = do{ pre  <- prefixP
                             ; x    <- term
                             ; post <- postfixP
                             ; return (post (pre x))
                             }

              postfixP   = postfixOp <|> return id

              prefixP    = prefixOp <|> return id

              rassocP x  = do{ f <- rassocOp
                             ; y  <- do{ z <- termP; rassocP1 z }
                             ; return (f x y)
                             }
                           <|> ambigiousLeft
                           <|> ambigiousNon
                           -- <|> return x

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do{ f <- lassocOp
                             ; y <- termP
                             ; lassocP1 (f x y)
                             }
                           <|> ambigiousRight
                           <|> ambigiousNon
                           -- <|> return x

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do{ f <- nassocOp
                             ; y <- termP
                             ;    ambigiousRight
                              <|> ambigiousLeft
                              <|> ambigiousNon
                              <|> return (f x y)
                             }
                           -- <|> return x

           in  do{ x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
                   <?> "operator"
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)
