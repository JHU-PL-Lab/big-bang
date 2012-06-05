{-# LANGUAGE      TemplateHaskell
                , MultiParamTypeClasses
                , FlexibleInstances
                , FlexibleContexts
                , UndecidableInstances
                , ScopedTypeVariables
                #-}

{-|
  This module is designed to generalize the process of creating compositional
  ASTs.

  TODO: documentation
-}

module Utils.Language.Ast where

import Control.Monad.Identity
import qualified Utils.Language.AstMeta as Meta
import Utils.Render.Display

$( return $ concat $ map Meta.astDecls Meta.astArities )

-- |Performs an operation over an AST.  Instances of this typeclass are provided
--  by this module and invoke the @AstStep@ instances for the AST's members.
--  The @op@ argument is meant to provide a means by which different typeclass
--  instances can be disambiguated; it is passed verbatim to the @AstStep@
--  typeclass.
class AstOp op ast result where
  astop :: op -> ast -> result

-- |Performs an operation over a member of a partial AST type.  Users of this
--  module are expected to provide instances of this typeclass to define AST
--  operations.  Selection of the specific @AstStep@ instance is expected to be
--  driven by the @op@ argument.
class AstStep op part ast result where
  aststep :: op -> part ast -> result

$( return $ concat $ map Meta.opDecls Meta.astArities )

-- |Wraps a node in an AST wrapper.  This module provides implementations of
--  this typeclass for each AST wrapper type.
class AstWrap part ast where
  astwrap :: part ast -> ast

$( return $ concat $ map (\(i,j) -> Meta.wrapDecls i j)
    [(i,j) | j <- Meta.astArities, i <- [1..j] ] )

-- |Defines a monadic homomorphic operation over partial AST types.  Users of
--  this module should provide an instance of this typeclass when the partial
--  AST type is to be used in transformations relying on homomorphisms (such as
--  upcasting).  Homomorphism instances should be of the form
--  @
--      instance (AstWrap part, Monad m)
--            => AstStep HomOpM part ast1 ((ast1 -> m ast2) -> m ast2)
--  @
--  The function of type @ast1 -> ast2@ is to be used on each child node in the
--  partial AST type.
data HomOpM = HomOpM

-- |Defines a non-monadic homomorphic operation over partial AST types.  This
--  operation is provided for convenience; it uses the Identity monad.  This
--  module provides the appropriate typeclass instance for any type for which
--  @HomOpM@ has been defined.
data HomOp = HomOp

-- |A typeclass instance for @HomOp@ given @HomOpM@.
instance (AstStep HomOpM part ast1 ((ast1 -> Identity ast2) -> Identity ast2))
      => AstStep HomOp part ast1 ((ast1 -> ast2) -> ast2) where
  aststep HomOp part = \f ->
    runIdentity $ aststep HomOpM part ((return . f)::(ast1 -> Identity ast2))

-- |Performs an upcasting operation on an AST over which homomorphisms are
--  defined.
--  NOTE: due to a bug present in at least GHC 7.4.1, the GHC compiler will
--        suggest an incorrect type signature for this function.  The type
--        signature shown here, in concert with the explicit annotation of
--        upcast in the definition, is sufficient.   The ticket for this bug
--        can be found at http://hackage.haskell.org/trac/ghc/ticket/6065
upcast :: forall ast1 ast2.
            (AstOp HomOp ast1 ((ast1 -> ast2) -> ast2)) => ast1 -> ast2
upcast ast = astop HomOp ast (upcast :: ast1 -> ast2)

-- |Defines a monadic catamorphic operation over partial AST types which
--  produces a monoidal result.  Users of this module may provide instances of
--  this typeclass for generalization of catamorphic operations (such as free
--  variable assessment) and then define specific catamorphisms by defining
--  cases for exceptions and then invoking the catamorphism as necessary.
--  Catamorphism instances should be of the form
--  @
--      instance (Monoid r, Monad m)
--            => AstStep CatOpM part ast ((ast -> m r) -> m r)
--  @
--  The function of type @ast -> m r@ is to be used on each child node in the
--  partial AST type; results are to be combined using @mappend@ and base
--  cases should be defined using @mempty@.
data CatOpM = CatOpM

-- |Defines a non-monadic catamorphic operation returning a monoid.  This
--  operation is provided for convenience; it uses the Identity monad.  This
--  module provides the appropriate typeclass instance for any type for which
--  @CatOpM@ has been defined.
data CatOp = CatOp

-- |A typeclass instance for @CatOp@ given @CatOpM@.
instance (AstStep CatOpM part ast ((ast -> Identity r) -> Identity r))
      => AstStep CatOp part ast ((ast -> r) -> r) where
  aststep CatOp part f =
    runIdentity $ aststep CatOpM part ((return . f)::(ast -> Identity r))

