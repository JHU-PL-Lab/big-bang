{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Test.HUnit
import System.Console.CmdArgs

import qualified Language.TinyBang.Config as Cfg
import qualified Language.LittleBang.Test.Onions as Onions
import qualified Language.LittleBang.Test.Functions as Functions
import qualified Language.LittleBang.Test.Case as Case
import qualified Language.LittleBang.Test.EagerOps as EagerOps
import qualified Language.LittleBang.Test.EmptyOnion as EmptyOnion
import qualified Language.LittleBang.Test.LazyOps as LazyOps
import qualified Language.LittleBang.Test.Lexer as Lexer
import qualified Language.LittleBang.Test.Misc as Misc
import qualified Language.LittleBang.Test.OnionSubtraction as OnionSubtraction
import qualified Language.LittleBang.Test.Parser as Parser
import qualified Language.LittleBang.Test.Peano as Peano
import qualified Language.LittleBang.Test.Primitive.Char as Primitive.Char
import qualified Language.LittleBang.Test.Primitive.Int as Primitive.Int
import qualified Language.LittleBang.Test.Primitive.Unit as Primitive.Unit
import qualified Language.LittleBang.Test.Projection as Projection
import qualified Language.LittleBang.Test.State as State
import qualified Language.LittleBang.Test.ListEncoding as ListEncoding

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
