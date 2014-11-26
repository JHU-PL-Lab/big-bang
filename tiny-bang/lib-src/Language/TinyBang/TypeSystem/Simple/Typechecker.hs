{-# LANGUAGE TemplateHaskell #-}
module Language.TinyBang.TypeSystem.Simple.Typechecker
( simpleTypeSystem
) where

import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Interface
import Language.TinyBang.TypeSystem.Simple.Closure
import Language.TinyBang.TypeSystem.Simple.InitialAlignment
import Language.TinyBang.TypeSystem.Simple.Polymorphism
import Language.TinyBang.TypeSystem.Simple.Variables
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

simpleTypeSystem :: TypeSystem
simpleTypeSystem =
  TypeSystem
    { typecheck = simpleTypecheck
    }

simpleTypecheck :: Expr -> TypecheckResult
simpleTypecheck expr =
  let (_,cs) = initiallyAlign expr in
  let f = polyinstFnForConstraintSetWithContour cs initialContour in
  let cs' = substituteVars f cs in
  _debugI (display $ text "Performing constraint closure on" <+> makeDoc cs') $
  let (incons,cs'') = computeClosure cs' in
  TypecheckResult
    { allConstraints = cs''
    , typeErrors = Set.map TypecheckInconsistent incons
    }
