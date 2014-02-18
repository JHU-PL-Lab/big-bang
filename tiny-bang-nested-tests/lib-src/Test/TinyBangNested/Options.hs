module Test.TinyBangNested.Options
( TinyBangNestedTestOptions(..)
, defaultTinyBangNestedTestOptions
, tinyBangNestedTestOptDescrs
) where

import Test.TinyBang.Options
import Utils.GetOpt

data TinyBangNestedTestOptions
  = TinyBangNestedTestOptions
      { tinyBangTestOptions :: TinyBangTestOptions
      }

defaultTinyBangNestedTestOptions :: TinyBangNestedTestOptions
defaultTinyBangNestedTestOptions =
  TinyBangNestedTestOptions
    { tinyBangTestOptions = defaultTinyBangTestOptions
    }

tinyBangNestedTestOptDescrs ::
    [OptDescr (OptionUpdater TinyBangNestedTestOptions)]
tinyBangNestedTestOptDescrs =
  let mappedTinyBangDescrs =
        map (mapUpdaterOptDescr tinyBangTestOptions
              (\r tbto -> r { tinyBangTestOptions = tbto }))
            tinyBangTestOptDescrs
  in
  mappedTinyBangDescrs

