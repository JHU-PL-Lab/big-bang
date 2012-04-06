{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Test.HUnit
import System.Console.CmdArgs

import qualified Language.MicroBang.Test.Onions as Onions
import qualified Language.MicroBang.Test.Functions as Functions
import qualified Language.MicroBang.Test.Case as Case
import qualified Language.MicroBang.Test.EagerOps as EagerOps
import qualified Language.MicroBang.Test.EmptyOnion as EmptyOnion
import qualified Language.MicroBang.Test.LazyOps as LazyOps
import qualified Language.MicroBang.Test.Misc as Misc
import qualified Language.MicroBang.Test.OnionSubtraction as OnionSubtraction
import qualified Language.MicroBang.Test.Parser as Parser
import qualified Language.MicroBang.Test.Peano as Peano
import qualified Language.MicroBang.Test.Primitive.Int as Primitive.Int
import qualified Language.MicroBang.Test.Primitive.Unit as Primitive.Unit
import qualified Language.MicroBang.Test.Projection as Projection
import qualified Language.MicroBang.Test.ListEncoding as ListEncoding

tests :: (?debug :: Bool) => [Test]
tests =
  [ Onions.tests
  , Functions.tests
  , Case.tests
  , EagerOps.tests
  , EmptyOnion.tests
  , LazyOps.tests
  --, Lexer.tests
  , Misc.tests
  , OnionSubtraction.tests
  , Parser.tests
  , Peano.tests
  --, Primitive.Char.tests
  , Primitive.Int.tests
  , Primitive.Unit.tests
  , Projection.tests
  --, State.tests
  , ListEncoding.tests
  ]

data Options = Options { debug :: Bool }
  deriving (Data, Typeable, Eq, Show)

defOpts :: Options
defOpts = Options {debug = def &= name "d"}

main :: IO Counts
main = do
  opts <- cmdArgs defOpts
  let ?debug = debug opts
  runTestTT $ TestList tests
