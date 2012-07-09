{-# LANGUAGE      TemplateHaskell
                , MultiParamTypeClasses
                , FlexibleInstances
                , FlexibleContexts
                , UndecidableInstances
                , ScopedTypeVariables
                , TypeOperators
                , TypeFamilies
                , KindSignatures
                #-}

{-|
  This module provides an extensible variant datatype.  This datatype is
  similar in some behavior to the coproduct-with-subsumption type that is
  defined in Data Types a la Carte: it permits a user to create a variant
  data type, specifically by naming a series of data types of kind * -> *
  which expect the recursive type as their argument.  For instance, a variant
  could be constructed as follows:

  @
    data Lit a = IntL Integer | UnitL
    data Arith a = Plus a a | Neg a
    type MathAst = Lit :|: Arith
    x :: MathAst = inj $ Neg $ inj $ Plus (inj $ IntL 2) (inj $ IntL 3)
  @

  For better legibility, smart constructors can be defined:

  @
    plus :: (Plus :<< t) => t -> t -> t
    plus x y = inj $ Plus x y
    ...
    x = neg $ plus (intL 2) (intL 3)
  @

  TODO: more examples, especially w.r.t. operations

  While somewhat similar as a fundamental data structure, the implementation
  is considerably different.  Instead of using an incremental construction
  model with subsumption, extensible variant types are flat and have fixed
  arity.  This is somewhat restrictive in that it imposes a maximum width for
  extensible variant width, but it also leads to somewhat simpler code which is
  more amenable to type inference.

  TODO: more docs
-}

module Data.ExtensibleVariant where

import Control.Monad.Identity
import qualified Data.ExtensibleVariant.Meta as Meta
import Language.Haskell.TH.Syntax
import Utils.Render.Display

$( return $ concat $ map Meta.xvDecls Meta.xvArities )

-- |Defines operations over an extensible variant structure.  Instances of this
--  typeclass are provided by this module; they invoke the @XvPart@ instances of
--  the variants' members.  The @op@ argument is meant to provide a means by
--  which different typeclass instances can be disambiguated; it is passed
--  verbatim to the @XvPart@ typeclass instance.
class XvOp op xv result where
  xvop :: op -> xv -> result

-- |Performs an operation over a component of an extensible variant structure.
--  Users of this module are expected to provide instances of this typeclass to
--  define variant operations.  Selection of the specific @XvPart@ instance is
--  typically driven by the @op@ argument.
class XvPart op comp xv result where
  xvpart :: op -> comp xv -> result

$( return $ concat $ map Meta.opDecls Meta.xvArities )

-- |Indicates containment of a component in an extensible variant type.  This
--  module provides implementations of this typeclass for each concrete
--  extensible variant type.
class (comp :: * -> *) :<< (xv :: *) where
  -- Injects a provided component into an extensible variant structure.
  inj :: comp xv -> xv
  -- Projects a given component type from an extensible variant structure.  If
  -- that component is not the top level of the structure, Nothing is returned.
  prj :: xv -> Maybe (comp xv)

$( return $ concat $ map (\(i,j) -> Meta.containDecls i j)
    [(i,j) | j <- Meta.xvArities, i <- [1..j] ] )

-- |Describes an infix operator which represents variant extension.  This
--  operator may be more legible than the constructor prefix equivalent.
type family (left :: *) :|| (right :: * -> *) :: *
infixl :||
-- |Describes an infix operator which is a synonym for Xv2.
type family (left :: * -> *) :|: (right :: * -> *) :: *
infixl :|:
type instance a :|: b = Xv2 a b

$( return $ concat $ map Meta.xvConstrFamilyDecls $
        filter (> 2) Meta.xvArities )

-- |Defines a monadic homomorphic operation over extensible variant component
--  types.  Users of this module should provide an instance of this typeclass
--  when the component is to be used in transformations relying on homomorphisms
--  (such as upcasting).  Homomorphism instances should be of the form
--  @
--      instance (XvWrap comp, Monad m)
--            => XvPart HomOpM comp xv1 ((xv1 -> m xv2) -> m xv2)
--  @
--  The function of type @xv1 -> m xv2@ is to be used on each child in the
--  component.
data HomOpM = HomOpM

-- |Defines a non-monadic homomorphic operation over extensible variant
--  component types.  This operation is provided for convenience; it uses the
--  Identity monad.  This module provides the appropriate typeclass instance
--  for any type for which @HomOpM@ has been defined.
data HomOp = HomOp

-- |A typeclass instance for @HomOp@ given @HomOpM@.
instance (XvPart HomOpM comp xv1 ((xv1 -> Identity xv2) -> Identity xv2))
      => XvPart HomOp comp xv1 ((xv1 -> xv2) -> xv2) where
  xvpart HomOp comp = \f ->
    runIdentity $ xvpart HomOpM comp ((return . f)::(xv1 -> Identity xv2))

-- |Performs an upcasting operation on an extensible variant over which
--  homomorphisms are defined.
--  NOTE: due to a bug present in at least GHC 7.4.1, the GHC compiler will
--        suggest an incorrect type signature for this function.  The type
--        signature shown here, in concert with the explicit annotation of
--        upcast in the definition, is sufficient.   The ticket for this bug
--        can be found at http://hackage.haskell.org/trac/ghc/ticket/6065
upcast :: forall xv1 xv2.
            (XvOp HomOp xv1 ((xv1 -> xv2) -> xv2)) => xv1 -> xv2
upcast xv = xvop HomOp xv (upcast :: xv1 -> xv2)

-- |Defines a monadic catamorphic operation over extensible variant component
--  types which produces a monoidal result.  Users of this module may provide
--  instances of this typeclass for generalization of catamorphic operations
--  (such as free variable assessment) and then define specific catamorphisms
--  by defining cases for exceptions and then invoking the catamorphism as
--  necessary.  Catamorphism instances should be of the form
--  @
--      instance (Monoid r, Monad m)
--            => XvPart CatOpM comp xv ((xv -> m r) -> m r)
--  @
--  The function of type @xv -> m r@ is to be used on each child node in the
--  component type; results are to be combined using @mappend@ and base
--  cases should be defined using @mempty@.
data CatOpM = CatOpM

-- |Defines a non-monadic catamorphic operation returning a monoid.  This
--  operation is provided for convenience; it uses the Identity monad.  This
--  module provides the appropriate typeclass instance for any type for which
--  @CatOpM@ has been defined.
data CatOp = CatOp

-- |A typeclass instance for @CatOp@ given @CatOpM@.
instance (XvPart CatOpM comp xv ((xv -> Identity r) -> Identity r))
      => XvPart CatOp comp xv ((xv -> r) -> r) where
  xvpart CatOp comp f =
    runIdentity $ xvpart CatOpM comp ((return . f)::(xv -> Identity r))

-- |A function for generating smary constructors on a given extensible variant
--  component type.  For each constructor in the specified type of the form
--  @Constr arg1 ... argN@, this metaprogram will generate a function
--  declaration of the form @inj $ constr arg1 ... argN@.
genSmartConstr :: Name -> Q [Dec]
genSmartConstr = Meta.genSmartConstr

