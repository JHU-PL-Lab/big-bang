{-# LANGUAGE TupleSections #-}

module Test.Data.NFA
( nfaTests
) where

import Data.NFA
import Test.HUnit

type TestNfa = Nfa Int Char

{-@
  (1) --a--> ((2))
@-}
example1 :: TestNfa
example1 = createFromData 1 [(1,'a',2)] [2]

{-|@
  (1) --a--> ((2))
    \          ^
     \         |
      b--> 3 --c
@-}
example2 :: TestNfa
example2 = createFromData 1 [(1,'a',2),(1,'b',3),(3,'c',2)] [2]

{-|@
  (1) --a--> ((2)) <---
               |       |
                -b,c,d-
@-}
example3 :: TestNfa
example3 = createFromData 1 ((1,'a',2):map (2,,2) "bcd") [2]

{-|@
  (1) --a--> 2 --c--> 3 --b--> 4 --d--> ((5))
@-}
example4 :: TestNfa
example4 = createFromData 1 (map (\(x,e) -> (x,e,x+1)) $ zip [1..] "acbd") [5]

{-|@
  (1)   ((2))
@-}
example5 :: TestNfa
example5 = createFromData 1 [] [2]

{-|@
  (1) ----> 2<-
   |        |  |
   |        b  |
   |        | /
   |        v/
    --a---> 3 --c--> ((4))
@-}
example6 :: TestNfa
example6 = createFromDataWithEpsilon 1
              [ (1,Nothing,2)
              , (1,Just 'a',3)
              , (2,Just 'b',3)
              , (3,Nothing,2)
              , (3,Just 'c',4) ] [4]

-- |The tests for this module.
nfaTests :: Test
nfaTests =
  let ex1 = ("example1",example1)
      ex2 = ("example2",example2)
      ex3 = ("example3",example3)
      ex4 = ("example4",example4)
      ex5 = ("example5",example5)
      ex6 = ("example6",example6)
      ex1or2 = ("ex1|ex2", example1 `union` example2)
      ex1and2 = ("ex1&ex2", example1 `intersect` example2)
      ex2and3 = ("ex2&ex3", example2 `intersect` example3)
      ex1and4 = ("ex1&ex4", example1 `intersect` example4)
      ex3and4 = ("ex3&ex4", example3 `intersect` example4)
      ex1and6 = ("ex1&ex6", example1 `intersect` example6)
      ex2and6 = ("ex2&ex6", example2 `intersect` example6)
      ex3and6 = ("ex3&ex6", example3 `intersect` example6)
  in
  TestList
    [ TestLabel "Simple acceptance" $ TestList $
            genContainTests ex1 ["a"] ["b","","ba","ab","aa"]
        ++  genContainTests ex2
            [ "a", "bc"]
            [ "abc", "ac", "bca", "ba", "", "b", "c"]
        ++  genContainTests ex3
            [ "a", "ab", "ac", "ad", "abcd", "abb", "abcdb", "abdcbdb", "accc" ]
            [ "", "aa", "b", "bb", "bcd", "bcda", "e" ]
        ++  genContainTests ex4
            [ "acbd"]
            [ "a", "ab", "ac", "ad", "abcd", "abb", "abcdb", "abdbcbbdb", "accc"
            , "", "aa", "b", "bb", "bcd", "bcda", "e" ]
        ++  genContainTests ex6
            [ "bc", "bbbc", "abc", "ac", "abbbc" ]
            [ "c", "aac", "ab", "abbbb", "" ]
    , TestLabel "Combined acceptance" $ TestList $
            genContainTests ex1or2 ["a", "bc"] ["", "aa", "abc", "bca", "cb"]
        ++  genContainTests ex1and2 ["a"] ["bc", "aa", ""]
        ++  genContainTests ex2and3 ["a"] ["ad", "abc", "bc", "ac", "acb"]
        ++  genContainTests ex1and4 [] ["a", "acbd", "cbd", "aa"]
        ++  genContainTests ex3and4 ["acbd"] ["acbdd", "a", "bcdd", "abc"]
        ++  genContainTests ex2and6 ["bc"] ["ac", "a", "abc"]
        ++  genContainTests ex3and6
            [ "abc", "abbbbc", "ac" ]
            [ "bc", "bbbc", "abcd", "acc", "acb", "" ]
    , TestLabel "Emptiness" $ TestList $
            map (emptinessTest False) [ex1,ex2,ex3,ex4]
        ++  [ emptinessTest True ex5
            , emptinessTest False ex1or2
            , emptinessTest False ex1and2
            , emptinessTest False ex2and3
            , emptinessTest True ex1and4
            , emptinessTest False ex3and4
            , emptinessTest True ex1and6
            , emptinessTest False ex2and6
            , emptinessTest False ex3and6
            ]
    ]
  
genContainTests :: (Show sy) => (String,Nfa st sy) -> [[sy]] -> [[sy]] -> [Test]
genContainTests target pos neg =
  map (genContainTest True target) pos ++ map (genContainTest False target) neg

genContainTest :: (Show sy) => Bool -> (String, Nfa st sy) -> [sy] -> Test
genContainTest cntn (name,nfa) str =
  TestLabel (makeMsg cntn)
      $ test $ assertBool (makeMsg $ not cntn) $ accept nfa str == cntn
  where
    makeMsg b = show str ++ " " ++ (if b then "not " else "") ++ "in " ++ name

emptinessTest :: (Ord st) => Bool -> (String, Nfa st sy) -> Test
emptinessTest empt (name,nfa) =
  TestLabel (makeMsg empt)
      $ test $ assertBool (makeMsg $ not empt) $ isEmpty nfa == empt
  where
    makeMsg b = name ++ " is " ++ (if b then "" else "not ") ++ "empty"
