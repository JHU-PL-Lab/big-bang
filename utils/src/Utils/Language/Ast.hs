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
class AstOp ast result where
  astop :: ast -> result

-- |Performs an operation over a member of a partial AST type.  Users of this
--  module are expected to provide instances of this typeclass to define AST
--  operations.  Selection of the specific @AstStep@ instance is expected to be
--  driven by the return type of this typeclass; instances are therefore
--  encouraged to produce shallow newtype wrappers.
class AstStep part ast result where
  astpart :: part ast -> result

$( return $ concat $ map Meta.opDecls Meta.astArities )

