{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Test.HUnit
import System.Console.CmdArgs

import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Test.Onions as Onions
import qualified Language.TinyBang.Test.Functions as Functions
import qualified Language.TinyBang.Test.Case as Case
import qualified Language.TinyBang.Test.EagerOps as EagerOps
import qualified Language.TinyBang.Test.EmptyOnion as EmptyOnion
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
import qualified Language.TinyBang.Test.ListEncoding as ListEncoding

tests :: (?conf :: Cfg.Config) => [Test]
tests =
  [ Onions.tests
  , Functions.tests
  , Case.tests
  , EagerOps.tests
  , EmptyOnion.tests
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
  , ListEncoding.tests
  ]

data Options = Options { debug :: Bool }
  deriving (Data, Typeable, Eq, Show)

defOpts :: Options
defOpts = Options {debug = def &= name "d"}

main :: IO Counts
main = do
  opts <- cmdArgs defOpts
  let ?conf = Cfg.Config { Cfg.debug = debug opts
                         , Cfg.typecheck = True
                         , Cfg.evaluate = True
                         }
  runTestTT $ TestList tests