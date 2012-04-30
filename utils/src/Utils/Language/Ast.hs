{-# LANGUAGE      TemplateHaskell
                , MultiParamTypeClasses
                , FlexibleInstances
                , FlexibleContexts
                , UndecidableInstances
                #-}

{-|
  This module is designed to generalize the process of creating compositional
  ASTs.

  TODO: documentation
-}

module Utils.Language.Ast where

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

-- |Defines a homomorphic operation over partial AST types.  Users of this
--  module should provide an instance of this typeclass when the partial AST
--  type is to be used in transformations relying on homomorphisms (such as
--  upcasting).  Homomorphism instances should be of the form
--  @
--      instance (AstOp HomOp ast1 ((ast1 -> ast2) -> ast2), AstWrap part)
--            => AstStep HomOp part ast1 ((ast1 -> ast2) -> ast2)
--  @
--  The function of type @ast1 -> ast2@ is to be used on each child node in the
--  partial AST type.
data HomOp = HomOp

-- |Performs an upcasting operation on an AST over which homomorphisms are
--  defined.
--  NOTE: due to a bug present in at least GHC 7.4.1, attaching a type
--        signature to this function causes a type error.  Leaving the
--        signature inferred seems to work.  The ticket for this bug can be
--        found at http://hackage.haskell.org/trac/ghc/ticket/6065
--upcast :: (AstOp HomOp ast1 ((ast1 -> ast2) -> ast2)) => ast1 -> ast2
upcast ast = astop HomOp ast upcast

