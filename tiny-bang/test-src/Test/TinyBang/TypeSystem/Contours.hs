{-|
  This test module tests the basic functionality of the contour operations.
-}
module Test.TinyBang.TypeSystem.Contours
( tests
) where

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Test.HUnit

tests :: Test
tests = 
  let c1 = (cn1,"cn1")
      c2 = (cn2,"cn2")
      c3 = (cn3,"cn3")
      c4 = (cn4,"cn4")
      c5 = (cn5,"cn5")
      c6 = (cn6,"cn6")
      c7 = (cn7,"cn7")
      c8 = (cn8,"cn8")
      c9 = (cn9,"cn9")
      c10 = (cn10,"cn10")
  in
  TestList
    [ makeOverlapTest True c1 c1
    , makeOverlapTest False c1 c2
    , makeOverlapTest False c2 c1
    , makeOverlapTest False c1 c3
    , makeOverlapTest True c1 c4
    , makeOverlapTest True c2 c4
    , makeOverlapTest True c4 c2
    , makeOverlapTest False c3 c4
    , makeOverlapTest True c4 c5
    , makeOverlapTest False c4 c6
    , makeOverlapTest True c5 c6
    , makeSubsumedByTest True c1 c1
    , makeSubsumedByTest False c1 c2
    , makeSubsumedByTest False c1 c3
    , makeSubsumedByTest True c1 c4
    , makeSubsumedByTest False c4 c1
    , makeSubsumedByTest False c3 c4
    , makeSubsumedByTest False c4 c5
    , makeSubsumedByTest False c5 c4
    , makeSubsumedByTest True c6 c5
    , makeSubsumedByTest False c4 c5
    , makeSubsumedByTest False c5 c6
    , makeSubsumedByTest True c7 c9
    , makeSubsumedByTest True c8 c9
    , makeSubsumedByTest False c9 c7
    , makeSubsumedByTest True c9 c8
    , makeSubsumedByTest True c9 c10
    , makeSubsumedByTest True c10 c9
    ]
  
makeTest :: String -> Bool -> Test
makeTest name cond =
  TestLabel name $ test $ assertBool "failed" cond
  
type NamedContour = (Contour, String)
  
makeOverlapTest :: Bool -> NamedContour -> NamedContour -> Test
makeOverlapTest expc (cn',n') (cn'', n'') =
  makeTest (n' ++ (if expc then " overlaps " else " does not overlap ") ++ n'')
    $ overlap cn' cn'' == expc
    
makeSubsumedByTest :: Bool -> NamedContour -> NamedContour -> Test
makeSubsumedByTest  expc (cn',n') (cn'', n'') =
  makeTest (n' ++ (if expc then " is subsumed by " else " is not subsumed by ")
               ++ n'')
    $ subsumedBy cn' cn'' == expc

cn1 :: Contour
cn1 = mkcn [x1]

cn2 :: Contour
cn2 = mkcn [x2]

cn3 :: Contour
cn3 = mkcn [x1']

cn4 :: Contour
cn4 = mkcn [x1,x2,x1] 

cn5 :: Contour
cn5 = mkcn [x1,x2,x3,x4,x2]

cn6 :: Contour
cn6 = mkcn [x1,x2,x3,x4]

cn7 :: Contour
cn7 = mkcn [x1,x2,x3]

cn8 :: Contour
cn8 = mkcn [x1,x2,x3,x2,x3]

-- cn9 ~= cn8
cn9 :: Contour
cn9 = union cn7 cn8

-- cn10 ~= cn9
cn10 :: Contour
cn10 = union cn9 cn1

mkcn :: [Var] -> Contour
mkcn = foldl (flip extend) initialContour
  
origin :: Origin
origin = ComputedOrigin []

x1 :: Var  
x1 = mkvar origin "x1"

x1' :: Var
x1' = Var origin (IdentifierVar "x1") $ Just 1

x2 :: Var
x2 = mkvar origin "x2"

x3 :: Var
x3 = mkvar origin "x3"

x4 :: Var
x4 = mkvar origin "x4"
