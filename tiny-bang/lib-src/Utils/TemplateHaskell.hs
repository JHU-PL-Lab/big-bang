module Utils.TemplateHaskell
( mkNames
) where

import Language.Haskell.TH

mkNames :: String -> Int -> [Name]
mkNames pfx count = map (mkName . (pfx ++) . show) [1 .. count]
