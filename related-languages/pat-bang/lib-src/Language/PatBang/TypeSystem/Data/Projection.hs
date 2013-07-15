{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Language.PatBang.TypeSystem.Data.Projection
( MultiProjection
, SingleProjection
) where

import Language.PatBang.Ast
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Types as T

-- |A type family describing the mapping from projector tags to projection
--  results.  Different forms of projector produce different types of result
--  as a part of the computational fibration model; this differs from the
--  fibration given in the specification because the naive declarative form is
--  exponentially complex.
type family MultiProjection db (tag :: ProjectorTag) :: *

-- |Primitive projection can simply result in a @(Bool, Fibration db)@
--  pair.  The @Bool@ just indicates whether or not the match was successful.
type instance MultiProjection db ProjPrimTag =
  (Bool, Fibration db)
{- |Given a source type variable and a label name, label projection produces a
--  triple between:
        * A list of variables which were found under labels of the given name
          in the onion structure of the source type variable.  The leftmost
          element in this list corresponds to the highest-priority projection.
        * A function which will, given one fibration for each of those
          variables, produce the overall fibration for that projection.  This
          list may be shorter or longer than the number of fibrations necessary.
          Longer lists are truncated while shorter lists are padded with
          @Unexpanded@ values.
        * A list of filtering fibrations for each of the variables.  These
          values may be @Unexpanded@; they may also be the remnants of a
          previously destructed filtering fibration used during the projection.
          If these fibrations are not @Unexpanded@, they must be observed by
          the caller.  (Callers which have no further refinement to do on the
          fibration can safely pass this list to the function described above.)
-}
type instance MultiProjection db ProjLabelTag =
  ([FlowTVar], [Fibration db] -> Fibration db, [Fibration db])
-- |Function projection produces the components of the function type and the
--  fibration which generated it.
type instance MultiProjection db ProjFunTag =
  ([([FlowTVar],FlowTVar,db)], Fibration db)
-- |Pattern projection produces the components of the pattern type and the
--  fibration which generated it.
type instance MultiProjection db ProjPatTag =
  ([([PatTVar],T.PatternBody)], Fibration db)
-- |Scape projection generates pairs between the scape variables and a function
--  which, given fibrations for those variables, will generate the overall
--  fibration.  This function operates as in the label case but takes pairs
--  (because there are two variables per scape).
type instance MultiProjection db ProjScapeTag =
  ( [(FlowTVar, FlowTVar)]
  , [(Fibration db, Fibration db)] -> Fibration db
  , [(Fibration db, Fibration db)] )
  
-- |A type family for single projection.  This type family degenerates its cases
--  expecting that only the highest-priority element is produced.
type family SingleProjection db (tag :: ProjectorTag) :: *

type instance SingleProjection db ProjPrimTag = (Bool, Fibration db)
type instance SingleProjection db ProjLabelTag =
  (FlowTVar, Fibration db -> Fibration db, Fibration db)
type instance SingleProjection db ProjFunTag =
  (([FlowTVar],FlowTVar,db), Fibration db)
type instance SingleProjection db ProjPatTag =
  (([PatTVar],T.PatternBody), Fibration db)
type instance SingleProjection db ProjScapeTag =
  ( (FlowTVar, FlowTVar)
  , (Fibration db, Fibration db) -> Fibration db
  , (Fibration db, Fibration db) )
