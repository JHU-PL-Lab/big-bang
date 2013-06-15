{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures #-}

module Language.TinyBang.TypeSystem.Data.Projection
( MultiProjection
, SingleProjection
) where

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types

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
--  @([CellTVar], [Fibration db] -> Fibration db)@.  The list of cell variables
--  describes those cells found under labels of the given name in the onion
--  structure of the source type variable.  The function will, given a fibration
--  for each of those variables, produce a fibration for the projection.  The
--  input list of fibrations is permitted to be shorter or longer than
--  necessary; if it is shorter, it is padded with @Unexpanded@ values.  Note
--  that the leftmost value in the lists corresponds to the highest-priority
--  projection.
type instance MultiProjection db ProjLabelTag =
  ([CellTVar], [Fibration db] -> Fibration db)
-- |Scape projection gets away with a @([Type db], Fibration db)@ where each
--  type in the list is a scape.  Note that the leftmost type is the highest
--  priority scape.
type instance MultiProjection db ProjFunTag =
  ([Type db], Fibration db)
  
-- |A type family for single projection.  This type family degenerates its cases
--  expecting that only the highest-priority element is produced.
type family SingleProjection db (tag :: ProjectorTag) :: *

type instance SingleProjection db ProjPrimTag = (Bool, Fibration db)
type instance SingleProjection db ProjLabelTag =
  (CellTVar, Fibration db -> Fibration db)
type instance SingleProjection db ProjFunTag = (Type db, Fibration db)

