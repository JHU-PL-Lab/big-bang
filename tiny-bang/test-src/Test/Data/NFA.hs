{-# LANGUAGE TupleSections #-}

module Test.Data.NFA
( nfaTests
) where

import qualified Language.TinyBang.Utils.Data.NFA as NFA
import Test.HUnit

type TestNfa = NFA.Nfa Char

{-|@
  (1) --a--> ((2))
@-}
example1 :: TestNfa
example1 = NFA.singleton 'a'

{-|@
  (1) --a--> ((2))
    \          ^
     \         |
      b--> 3 --c
@-}
example2 :: TestNfa
example2 =
  NFA.singleton 'a' `NFA.union` (NFA.singleton 'b' `NFA.concatenate` NFA.singleton 'c')

{-|@
  (1) --a--> ((2)) <---
               |       |
                -b,c,d-
@-}
example3 :: TestNfa
example3 =
  NFA.singleton 'a' `NFA.concatenate` NFA.kleeneSingleton ['b','c','d']

{-|@
  (1) --a--> 2 --c--> 3 --b--> 4 --d--> ((5))
@-}
example4 :: TestNfa
example4 =
  NFA.singleton 'a' `NFA.concatenate` NFA.singleton 'c' `NFA.concatenate`
    NFA.singleton 'b' `NFA.concatenate` NFA.singleton 'd'

{-|@
  (1)   ((2))
@-}
example5 :: TestNfa
example5 = NFA.empty

{-|@
  (1) ----> 2<-
   |        |  |
   |        b  |
   |        | /
   |        v/
    --a---> 3 --c--> ((4))
@-}
example6 :: TestNfa
example6 =
  NFA.optional (NFA.singleton 'a') `NFA.concatenate`
    NFA.kleeneStar (NFA.singleton 'b') `NFA.concatenate` NFA.singleton 'c'

-- |The tests for this module.
nfaTests :: Test
nfaTests =
  let ex1 = ("example1",example1)
      ex2 = ("example2",example2)
      ex3 = ("example3",example3)
      ex4 = ("example4",example4)
      ex5 = ("example5",example5)
      ex6 = ("example6",example6)
      ex1or2 = ("ex1|ex2", example1 `NFA.union` example2)
      ex1and2 = ("ex1&ex2", example1 `NFA.intersect` example2)
      ex2and3 = ("ex2&ex3", example2 `NFA.intersect` example3)
      ex1and4 = ("ex1&ex4", example1 `NFA.intersect` example4)
      ex3and4 = ("ex3&ex4", example3 `NFA.intersect` example4)
      ex1and6 = ("ex1&ex6", example1 `NFA.intersect` example6)
      ex2and6 = ("ex2&ex6", example2 `NFA.intersect` example6)
      ex3and6 = ("ex3&ex6", example3 `NFA.intersect` example6)
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
  
genContainTests :: (Show sy, Ord sy)
                => (String, NFA.Nfa sy) -> [[sy]] -> [[sy]] -> [Test]
genContainTests target pos neg =
  map (genContainTest True target) pos ++ map (genContainTest False target) neg

genContainTest :: (Show sy, Ord sy)
               => Bool -> (String, NFA.Nfa sy) -> [sy] -> Test
genContainTest cntn (name,nfa) str =
  TestLabel (makeMsg cntn)
      $ test $ assertBool (makeMsg $ not cntn) $ NFA.accept nfa str == cntn
  where
    makeMsg b = show str ++ " " ++ (if b then "not " else "") ++ "in " ++ name

emptinessTest :: (Ord sy) => Bool -> (String, NFA.Nfa sy) -> Test
emptinessTest empt (name,nfa) =
  TestLabel (makeMsg empt)
      $ test $ assertBool (makeMsg $ not empt) $ NFA.isEmpty nfa == empt
  where
    makeMsg b = name ++ " is " ++ (if b then "" else "not ") ++ "empty"
