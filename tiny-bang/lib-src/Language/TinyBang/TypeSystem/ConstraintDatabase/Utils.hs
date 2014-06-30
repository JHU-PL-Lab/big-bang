{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils
( queryLowerBoundsOfTypeOrVar
, SomeConstraintDatabase(..)
, SomeDisplayableConstraintDatabase(..)
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display

queryLowerBoundsOfTypeOrVar :: (ConstraintDatabase db)
                            => db -> TypeOrVar db -> Set (Type db)
queryLowerBoundsOfTypeOrVar db tov =
  case unTypeOrVar tov of
    Left t -> Set.singleton t
    Right a -> query db $ QueryLowerBoundingTypesOfTVar a

data SomeConstraintDatabase where
  SomeConstraintDatabase :: forall db. (ConstraintDatabase db)
                         => db -> SomeConstraintDatabase

data SomeDisplayableConstraintDatabase where
  SomeDisplayableConstraintDatabase
    :: forall db. (ConstraintDatabase db, Display db)
    => db
    -> SomeDisplayableConstraintDatabase
