{-# LANGUAGE TemplateHaskell, TupleSections, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving, UndecidableInstances, LambdaCase, ScopedTypeVariables #-}

{-|
  A 'ConstraintDatabase' implementation which uses indexed data structures
  to accelerate queries.  This technique is effective on reads but does not do
  much to improve performance on substitutions (such as in polyinstantiation).
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Indexed
( IndexedConstraintDatabase(..)
) where

import Control.Monad
import Data.EitherR
import Data.IndexedSet as IS
import Data.IndexedSet.Common
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Language.Haskell.TH

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface as CDb
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FindVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FreeVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.Polyinst
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.ReplaceVars
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours as Cntr
import Language.TinyBang.TypeSystem.Types as TBT
import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Language.TinyBang.Utils.TemplateHaskell.Reduce
import Language.TinyBang.Utils.TemplateHaskell.Transform

$(loggingFunctions)

-- |A utility function necessary because tuple sections cannot be parsed within
--  Template Haskell slices.
preunit :: a -> ((),a)
preunit = ((),)

-- |The Template Haskell definition of the indexed constraint set used to back
--  the database.
$(let dbType = conT $ mkName "IndexedConstraintDatabase" in
  createIndexedSet
    "IndexedConstraintSet" -- data type name
    "IndexedConstraintSetQuery" -- query type name
    [t|Constraint $(dbType)|] -- set element type
    -- The following are the descriptors for the queries we wish to define
    [ QueryDescriptor
        { queryName = "IxAllTVars"
        , queryInputType = [t|()|]
        , queryOutputType = [t|TVar|]
        , queryAnalyzer =
            [| map preunit . Set.toList . findAllVars |]
        }
    , QueryDescriptor
        { queryName = "IxAllTypesLowerBoundingVars"
        , queryInputType = [t|()|]
        , queryOutputType = [t|(TBT.Type $(dbType), TVar)|]
        , queryAnalyzer =
            [| \case
                  TypeConstraint _ t a -> [preunit (t,a)]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxAllApplications"
        , queryInputType = [t|()|]
        , queryOutputType = [t|(TVar, TVar, TVar)|]
        , queryAnalyzer =
            [| \case
                  ApplicationConstraint _ a1 a2 a3 -> [preunit (a1,a2,a3)]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxAllBuiltins"
        , queryInputType = [t|()|]
        , queryOutputType = [t|(BuiltinOp, [TVar], TVar)|]
        , queryAnalyzer =
            [| \case
                  BuiltinConstraint _ op as a -> [preunit (op,as,a)]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxAllInconsistencies"
        , queryInputType = [t|()|]
        , queryOutputType = [t|Inconsistency $(dbType)|]
        , queryAnalyzer =
            [| \case
                  InconsistencyConstraint _ i -> [preunit i]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxLowerBoundingTypesOfTVar"
        , queryInputType = [t|TVar|]
        , queryOutputType = [t|TBT.Type $(dbType)|]
        , queryAnalyzer =
            [| \case
                  TypeConstraint _ t a -> [(a,t)]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxUpperBoundingTVarsOfTVar"
        , queryInputType = [t|TVar|]
        , queryOutputType = [t|TVar|]
        , queryAnalyzer =
            [| \case
                  IntermediateConstraint _ a1 a2 -> [(a1,a2)]
                  _ -> [] |]
        }
    , QueryDescriptor
        { queryName = "IxAllContours"
        , queryInputType = [t|()|]
        , queryOutputType = [t|Contour|]
        , queryAnalyzer =
            [| map preunit . mapMaybe contourOfVar . Set.toList . findAllVars |]
        }
    ]
 )

deriving instance Eq IndexedConstraintSet
deriving instance Ord IndexedConstraintSet
deriving instance Show IndexedConstraintSet

-- |The wrapper type used to represent the database.
newtype IndexedConstraintDatabase
  = IndexedConstraintDatabase IndexedConstraintSet
  deriving (Eq, Ord, Show)
  
unIndexedConstraintDatabase :: IndexedConstraintDatabase -> IndexedConstraintSet
unIndexedConstraintDatabase (IndexedConstraintDatabase cset) = cset

instance Display IndexedConstraintDatabase where
  makeDoc = makeDoc . IS.toSet . unIndexedConstraintDatabase

-- |Declares the empty and union definitions for 'IndexedConstraintDatabase'.
instance Monoid IndexedConstraintDatabase where
  mempty = IndexedConstraintDatabase IS.empty
  mappend db1 db2 =
    assertWellFormed $
    either absurd id $ runEitherR $
      do -- EitherR
        let db1cs = unIndexedConstraintDatabase db1
        let db2cs = unIndexedConstraintDatabase db2
        -- Start by calculating the contours in each database
        let (db1Cntrs, db2Cntrs) = (contoursOfDb db1, contoursOfDb db2)
        -- If there are no novel contours, just use the easy implementation
        when (Set.null (db2Cntrs Set.\\ db1Cntrs)
           || Set.null (db1Cntrs Set.\\ db2Cntrs) ) $
          succeed $ unionWithExistingContours db1 db2
        -- Next, merge any overlapping contours
        let allCntrs = Set.union db1Cntrs db2Cntrs
        let mergedCntrs =
              Set.map (`mergeIfOverlap` Set.toList allCntrs) allCntrs
        -- And filter out any contours which are subsumed by the new ones
        let disjointCntrs = Set.filter
              (\cn -> not $ any (cn `strictlySubsumedBy`) $
                        Set.toList mergedCntrs)
              mergedCntrs
        -- Also filter out any contours which we already know about
        let interestingCntrs = Set.filter (`Set.member` allCntrs) disjointCntrs
        -- Create the new set of constraints (since the indices won't be useful
        -- anymore)
        -- PERF: It's not impossible to reuse at least some of the indices,
        --       even with the TH-generated indexed set.  But we'd have to
        --       specialize that step.
        let cs = Set.union (IS.toSet db1cs) (IS.toSet db2cs)
        -- Log the contours which have been found.  This can be done as a post
        -- step because EitherR is strict enough.
        _debugM $ display $
          text "Unioning two indexed constraint databases:" <> line <>
            indent 2 (align $
              text "Contours in db1:  " <+> makeDoc db1Cntrs <> line <>
              text "Contours in db2:  " <+> makeDoc db2Cntrs <> line <>
              text "Merged contours:  " <+> makeDoc mergedCntrs <> line <>
              text "Disjoint contours:" <+> makeDoc disjointCntrs <> line <>
              text "Interesting contours:" <+> makeDoc interestingCntrs)
        -- This new set of contours is disjoint and each contour in it entirely
        -- subsumes any contour in the constraint database that it overlaps.
        -- Therefore, any contour in the database which overlaps a contour in
        -- this new set should be replaced by the latter contour.  Do that now.
        -- TODO: remove the following debug logs (or clean them up)
        succeed $ IndexedConstraintDatabase $ IS.fromSet $
          replaceVarsByContours interestingCntrs cs
    where
      mergeIfOverlap :: Contour -> [Contour] -> Contour
      mergeIfOverlap cntr others =
        foldl Cntr.union cntr $
          filter (\cntr' -> overlap cntr cntr' && cntr /= cntr') others
      strictlySubsumedBy :: Contour -> Contour -> Bool
      strictlySubsumedBy cn1 cn2 =
        postLog _debugI
          (\r -> display $ text "Strict subsumption check: " <+> makeDoc cn1 <+>
                  text (if r then "is" else "is not") <+>
                  text "strictly subsumed by" <+> makeDoc cn2) $
          cn1 `subsumedBy` cn2 && not (cn2 `subsumedBy` cn1)

-- |Declares the 'ConstraintDatabase' instance for 'IndexedConstraintDatabase'.
instance ConstraintDatabase IndexedConstraintDatabase where
  add c db =
    mappend db $ IndexedConstraintDatabase $ IS.singleton c
  
  addWithExistingContours c db =
    assertWellFormed $
      IndexedConstraintDatabase $ IS.insert c $ unIndexedConstraintDatabase db

  unionWithExistingContours a b =
    assertWellFormed $
      IndexedConstraintDatabase $
        unIndexedConstraintDatabase a `IS.union` unIndexedConstraintDatabase b

  polyinstantiate = polyinst
  
  query db q =
    -- Either between an answer and an indexed set query
    let cs = unIndexedConstraintDatabase db in
    case q of
      QueryAllConstraints ->
        IS.toSet cs
      QueryAllTVars ->
        stdQuery $ QueryIxAllTVars ()
      QueryAllFreeTVars ->
        -- PERF: do something cleverer here
        findFreeVars $ IS.toSet cs
      QueryAllTypesLowerBoundingTVars ->
        stdQuery $ QueryIxAllTypesLowerBoundingVars ()
      QueryAllApplications ->
        stdQuery $ QueryIxAllApplications ()
      QueryAllBuiltins ->
        stdQuery $ QueryIxAllBuiltins ()
      QueryAllInconsistencies ->
        stdQuery $ QueryIxAllInconsistencies ()
      QueryLowerBoundingTypesOfTVar a ->
        stdQuery $ QueryIxLowerBoundingTypesOfTVar a
      QueryUpperBoundingTVarsOfTVar a ->
        stdQuery $ QueryIxUpperBoundingTVarsOfTVar a
    where
      stdQuery :: forall r. IndexedConstraintSetQuery r -> Set r
      stdQuery = IS.query $ unIndexedConstraintDatabase db

-- |Declares a variable finding reduction for the indexed constraint database.
instance Reduce FindAllVars IndexedConstraintDatabase (Set TVar) where
  reduce FindAllVars db =
    IS.query (unIndexedConstraintDatabase db) (QueryIxAllTVars ())

-- |Declares a polyinstantiating transformation for the indexed constraint
--  database.
instance Transform PolyInst IndexedConstraintDatabase where
  transform p db =
    IndexedConstraintDatabase $ IS.fromSet $ transform p $ IS.toSet $
      unIndexedConstraintDatabase db 

-- |Declares a contour-replacing transformation for the indexed constraint
--  database.
instance Transform ReplaceVars IndexedConstraintDatabase where
  transform p db =
    IndexedConstraintDatabase $ IS.fromSet $ transform p $ IS.toSet $
      unIndexedConstraintDatabase db

-- |Declares a free variable reduction for the indexed constriant database.
instance Reduce FindFreeVars IndexedConstraintDatabase FindFreeVarsResult where
  reduce ffv db = reduce ffv $ IS.toSet $ unIndexedConstraintDatabase db

-- Assertion utilities ---------------------------------------------------------

-- |Fetches all of the contours appearing in an indexed constraint database.
contoursOfDb :: IndexedConstraintDatabase -> Set Contour
contoursOfDb db =
  let cntrs =  IS.query (unIndexedConstraintDatabase db) $
                  QueryIxAllContours () in
  let cntrs' = Set.map fromJust $ Set.filter isJust $ Set.map contourOfVar $
                  findAllVars $ IS.toSet $ unIndexedConstraintDatabase db in
  assertWithMessage
    (display $
        text "Indexed constraint database has incorrect cached contour set:" <>
        line <>
        indent 2 (align $
          text "Cached set:" <+> makeDoc cntrs <> line <>
          text "Actual set:" <+> makeDoc cntrs'
          ))
    (cntrs == cntrs')
    cntrs

-- |Ensures that the database is "well-formed" -- that is, that none of the
--  contours appearing within it overlap with each other.
assertWellFormed :: IndexedConstraintDatabase -> IndexedConstraintDatabase
assertWellFormed db =
  -- We'll do this by forming a list of assertion functions and then chaining
  -- them together.  Then, any arriving argument must trigger all of these
  -- functions in sequence in order to make it to the other side.
  let cntrs = Set.toList $ contoursOfDb db in
  (foldl (.) id $ map ensureNoneOverlapping $ inits cntrs) db
  where
    ensureNotOverlapping :: Contour
                         -> Contour
                         -> IndexedConstraintDatabase
                         -> IndexedConstraintDatabase
    ensureNotOverlapping cn1 cn2 db' =
      assertWithMessage
        (display $
          text "Two contours in the same indexed constraint database overlapped!" <> line <>
          (indent 2 $ align $
            text "Contour 1:" <+> makeDoc cn1 <> line <> 
            text "Contour 2:" <+> makeDoc cn2 <> line <>
            text "Database: " <+> makeDoc db'))
        (not $ cn1 `overlap` cn2)
        db'
    ensureNoneOverlapping :: [Contour]
                          -> IndexedConstraintDatabase
                          -> IndexedConstraintDatabase
    ensureNoneOverlapping cntrs =
      case cntrs of
        [] -> id
        _:[] -> id
        cntr:cntrs' -> foldl1 (.) $ map (ensureNotOverlapping cntr) cntrs'
