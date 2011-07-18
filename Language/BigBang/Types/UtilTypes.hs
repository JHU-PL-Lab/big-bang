{- |A module containing Haskell types which are used as utilities by other
    modules.
-}
module Language.BigBang.Types.UtilTypes
( LabelName
, labelName
, unLabelName
, Ident
, ident
, unIdent
) where

-- |A distinguished type for labels.
newtype LabelName = LabelName { unLabelName :: String }
    deriving (Eq, Ord, Show)
{- TODO: smarter constructor -}
labelName s = LabelName s

-- |A distinguished type for identifiers.
newtype Ident = Ident { unIdent :: String }
    deriving (Eq, Ord, Show)
{- TODO: smarter constructor -}
ident s = Ident s
