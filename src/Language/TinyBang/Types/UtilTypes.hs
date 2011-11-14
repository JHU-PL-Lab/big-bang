{- |A module containing Haskell types which are used as utilities by other
    modules.
-}
module Language.TinyBang.Types.UtilTypes
( LabelName
, labelName
, unLabelName
, Ident
, ident
, unIdent
) where

import Language.TinyBang.Render.Display

-- |A distinguished type for labels.
newtype LabelName = LabelName { unLabelName :: String }
    deriving (Eq, Ord, Show)
labelName :: String -> LabelName
{- TODO: smarter constructor -}
labelName s = LabelName s

-- |A distinguished type for identifiers.
newtype Ident = Ident { unIdent :: String }
    deriving (Eq, Ord, Show)
ident :: String -> Ident
{- TODO: smarter constructor -}
ident s = Ident s

instance Display LabelName where
    makeDoc = text . unLabelName

instance Display Ident where
    makeDoc = text . unIdent