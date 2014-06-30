module Test.LittleBang.Options
( LittleBangTestOptions(..)
, defaultLittleBangTestOptions
, littleBangTestOptDescrs
) where

import Test.TinyBang.Options
import Utils.GetOpt

data LittleBangTestOptions
  = LittleBangTestOptions
      { tinyBangTestOptions :: TinyBangTestOptions
      }

defaultLittleBangTestOptions :: LittleBangTestOptions
defaultLittleBangTestOptions =
  LittleBangTestOptions
    { tinyBangTestOptions = defaultTinyBangTestOptions
    }

littleBangTestOptDescrs :: [OptDescr (OptionUpdater LittleBangTestOptions)]
littleBangTestOptDescrs =
  let mappedTinyBangDescrs =
        map (mapUpdaterOptDescr tinyBangTestOptions
              (\r tbto -> r { tinyBangTestOptions = tbto }))
            tinyBangTestOptDescrs
  in
  mappedTinyBangDescrs

