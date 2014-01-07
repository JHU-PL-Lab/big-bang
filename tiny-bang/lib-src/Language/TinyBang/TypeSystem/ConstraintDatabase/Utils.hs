module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils
( queryLowerBoundsOfTypeOrVar
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.Types

queryLowerBoundsOfTypeOrVar :: (ConstraintDatabase db)
                            => db -> TypeOrVar db -> Set (Type db)
queryLowerBoundsOfTypeOrVar db tov =
  case unTypeOrVar tov of
    Left t -> Set.singleton t
    Right a -> query db $ QueryLowerBoundingTypesOfTVar a
