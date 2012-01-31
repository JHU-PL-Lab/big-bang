module Main where

import Test.HUnit

import qualified Language.TinyBang.Test.Onions as Onions
import qualified Language.TinyBang.Test.Functions as Functions
import qualified Language.TinyBang.Test.Binders as Binders
import qualified Language.TinyBang.Test.Case as Case
import qualified Language.TinyBang.Test.EagerOps as EagerOps
import qualified Language.TinyBang.Test.LazyOps as LazyOps
import qualified Language.TinyBang.Test.Lexer as Lexer
import qualified Language.TinyBang.Test.Misc as Misc
import qualified Language.TinyBang.Test.OnionSubtraction as OnionSubtraction
import qualified Language.TinyBang.Test.Parser as Parser
import qualified Language.TinyBang.Test.Peano as Peano
import qualified Language.TinyBang.Test.Primitive.Char as Primitive.Char
import qualified Language.TinyBang.Test.Primitive.Int as Primitive.Int
import qualified Language.TinyBang.Test.Primitive.Unit as Primitive.Unit
import qualified Language.TinyBang.Test.Projection as Projection
import qualified Language.TinyBang.Test.State as State

tests =
  [ Onions.tests
  , Functions.tests
  , Binders.tests
  , Case.tests
  , EagerOps.tests
  , LazyOps.tests
  , Lexer.tests
  , Misc.tests
  , OnionSubtraction.tests
  , Parser.tests
  , Peano.tests
  , Primitive.Char.tests
  , Primitive.Int.tests
  , Primitive.Unit.tests
  , Projection.tests
  , State.tests
  ]

main = runTestTT $ TestList tests
