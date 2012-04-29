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

