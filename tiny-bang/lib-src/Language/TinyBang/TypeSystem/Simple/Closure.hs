{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, TemplateHaskell #-}
module Language.TinyBang.TypeSystem.Simple.Closure
( computeClosure
) where

import Language.TinyBang.TypeSystem.Simple.Closure.Rules
