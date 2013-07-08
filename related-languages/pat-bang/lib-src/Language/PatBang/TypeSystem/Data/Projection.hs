{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures #-}

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
-- |Given a source type variable and a label name, label projection produces a
--  @([FlowTVar], [Fibration db] -> Fibration db)@.  The list of cell variables
--  describes those cells found under labels of the given name in the onion
--  structure of the source type variable.  The function will, given a fibration
--  for each of those variables, produce a fibration for the projection.  The
--  input list of fibrations is permitted to be shorter or longer than
--  necessary; if it is shorter, it is padded with @Unexpanded@ values.  Note
--  that the leftmost value in the lists corresponds to the highest-priority
--  projection.
type instance MultiProjection db ProjLabelTag =
  ([FlowTVar], [Fibration db] -> Fibration db)
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
  ([(FlowTVar, FlowTVar)], [(Fibration db, Fibration db)] -> Fibration db)
  
-- |A type family for single projection.  This type family degenerates its cases
--  expecting that only the highest-priority element is produced.
type family SingleProjection db (tag :: ProjectorTag) :: *

type instance SingleProjection db ProjPrimTag = (Bool, Fibration db)
type instance SingleProjection db ProjLabelTag =
  (FlowTVar, Fibration db -> Fibration db)
type instance SingleProjection db ProjFunTag =
  (([FlowTVar],FlowTVar,db), Fibration db)
type instance SingleProjection db ProjPatTag =
  (([PatTVar],T.PatternBody), Fibration db)
type instance SingleProjection db ProjScapeTag =
  ((FlowTVar, FlowTVar), (Fibration db, Fibration db) -> Fibration db)

