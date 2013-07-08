{-|
  This module defines fibrations in the PatBang implementation.
  
  It should be noted that the fibrations as they are defined here are treated
  slightly differently than in the document.  In the document, the non-fibration
  is present only to ensure that fibrations are always finite; no relation makes
  use of them or matches on them.  In this implementation, the @Unexpanded@
  constructor denotes a sort of fibration wildcard; as a result, it is
  compatible with any existing decision.
  
  The specification defines relations which, given a single correct fibration,
  can define projection, compatibility, and so on.  In order to decide the
  correct fibration, this implementation instead refines fibrations
  incrementally; each stage calculates the decisions it requires and these
  requirements are merged with those of its peers.  A merge failure indicates
  that no fibration exists which will satisfy the relation.
-}
module Language.PatBang.TypeSystem.Fibrations
( Fibration(..)
, mergeFibrations
, blankFibrationFor
, unexpandeds
) where

import Control.Applicative  ((<$>))
import Control.Monad (zipWithM)

import Language.PatBang.Display
import Language.PatBang.TypeSystem.Types
import Language.PatBang.TypeSystem.Utils.DocumentContainer

-- |A data structure representing fibrations.  Each fibration level describes
--  a single value-level decision of lower bound.  The type parameter @db@
--  is used to describe the type of constraint database that may appear within
--  types named by the fibration.
data Fibration db
  = Unexpanded
  | Fibration (Type db) [Fibration db]
  deriving (Eq, Ord, Show)

-- |Attempts to merge two fibrations.  The merge of two fibrations is a
--  fibration which specifies a superset of the original fibrations' decisions.
--  If the two input fibrations make conflicting decisions, this function will
--  return Nothing.
mergeFibrations :: (Eq db)
                => Fibration db -> Fibration db -> Maybe (Fibration db)
mergeFibrations f1 f2 =
  case (f1,f2) of
    (_,Unexpanded) -> Just f1
    (Unexpanded,_) -> Just f2
    (Fibration t1 fs1, Fibration t2 fs2) ->
      if t1 /= t2
        then Nothing
        else Fibration t1 <$> zipWithM mergeFibrations fs1 fs2

-- |Given a lower-bounding type, this function will produce a "blank" fibration
--  for it: one which does not indicate any exploration.
blankFibrationFor :: Type db -> Fibration db
blankFibrationFor t = Fibration t $ case t of
  Primitive _ -> []
  EmptyOnion -> []
  Label _ _ -> [Unexpanded]
  Onion _ _ -> [Unexpanded, Unexpanded]
  Function _ _ _ -> []
  Pattern _ _ -> []
  Scape _ _ -> [Unexpanded, Unexpanded]
  
-- |A list of unexpanded values.
unexpandeds :: [Fibration db]
unexpandeds = Unexpanded : unexpandeds

instance (Display db, DocumentContainer db) => Display (Fibration db) where
  makeDoc fib = case fib of
    Unexpanded -> char '*'
    Fibration typ fibs -> parens $ makeDoc typ <+> char ',' <+> makeDoc fibs