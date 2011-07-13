{- |A module containing Haskell types which are used as utilities by other
    modules.
-}
module UtilTypes
( LabelName
, labelName
, unLabelName
, Ident
, ident
, unIdent
) where

-- |A distinguished type for labels.
newtype LabelName = LabelName { unLabelName :: String }
    deriving (Eq, Ord)
{- TODO: smarter constructor -}
labelName s = LabelName s

-- |A distinguished type for identifiers.
newtype Ident = Ident { unIdent :: String }
{- TODO: smarter constructor -}
ident s = Ident s
