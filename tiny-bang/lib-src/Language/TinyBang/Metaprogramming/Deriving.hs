{-# LANGUAGE TemplateHaskell, FlexibleInstances, TupleSections #-}

{-|
  This module provides a set of useful Template Haskell routines for deriving
  unusual typeclass instances.
-}

module Language.TinyBang.Metaprogramming.Deriving
( deriveEqSkipFirst
, deriveOrdSkipFirst
) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Monoid (mconcat)
import Language.Haskell.TH

-- |Derives an instance of @Eq@ for a given type but skips the first argument
--  in each constructor.  Each constructor must have at least one argument.
deriveEqSkipFirst :: Name -> Q [Dec]
deriveEqSkipFirst name = do
  info <- reify name
  [d|instance Eq $(mkInstanceType name) where
      a == b = $(mkCaseExpr 'a 'b)|]
  where
    mkCaseExpr :: Name -> Name -> Q Exp
    mkCaseExpr a b = do
      (_,cs) <- getBindersAndCons name
      let clauses = map mkCaseClause cs
      let defaultClause = match (tupP [wildP,wildP]) (normalB [|False|]) []
      caseE [|($(varE a), $(varE b))|] $ clauses ++
        if length cs > 1 then [defaultClause] else []
    mkCaseClause :: Con -> Q Match
    mkCaseClause con = do
      (cname,argnum) <- conNameAndArgNum con
      when (argnum < 1) $ fail $
          "Constructor " ++ show cname ++ " has zero arguments"
      names1 <- sequence $ take (argnum-1) $ mkArgNames "a"
      names2 <- sequence $ take (argnum-1) $ mkArgNames "b"
      (pat, e) <- patAndExp cname names1 names2
      return $ Match pat (NormalB e) []
      where
        patAndExp :: Name -> [Name] -> [Name] -> Q (Pat, Exp)
        patAndExp cname a1names a2names = do
          let pat = TupP [patPart a1names, patPart a2names]
          e <- foldl expJoin [|True|] $ map expPart $ zip a1names a2names
          return (pat,e)
          where
            patPart :: [Name] -> Pat
            patPart anames =
              ConP cname $ WildP : map VarP anames
            expPart :: (Name,Name) -> Q Exp
            expPart (aname1,aname2) = [|$(varE aname1) == $(varE aname2)|]
            expJoin :: Q Exp -> Q Exp -> Q Exp
            expJoin a b = [|$a && $b|]

-- |Derives an instance of @Ord@ for a given type but skips the first argument
--  in each constructor.  Each constructor must have at least one argument.
deriveOrdSkipFirst :: Name -> Q [Dec]
deriveOrdSkipFirst name = do
  info <- reify name
  [d|instance Ord $(mkInstanceType name) where
      compare a b = $(mkCaseExpr 'a 'b)|]
  where
    mkCaseExpr :: Name -> Name -> Q Exp
    mkCaseExpr a b = do
      (_,cs) <- getBindersAndCons name
      let ncs = zip cs [(1::Int)..]
      let clauses = map makeCaseClause [(x,y) | x <- ncs, y <- ncs]
      caseE [|($(varE a), $(varE b))|] clauses
    makeCaseClause :: ((Con,Int),(Con,Int)) -> Q Match
    makeCaseClause ((cona,idxa),(conb,idxb)) = do
      (namea, argnuma) <- conNameAndArgNum cona
      (nameb, argnumb) <- conNameAndArgNum conb
      when (argnuma < 1) $ fail $
          "Constructor " ++ show namea ++ " has zero arguments"
      when (argnumb < 1) $ fail $
          "Constructor " ++ show nameb ++ " has zero arguments"
      let blankpat = TupP [ ConP namea $ replicate argnuma WildP
                          , ConP nameb $ replicate argnumb WildP ]
      (pat,e) <- case idxa `compare` idxb of
        LT -> (blankpat,) <$> [|LT|]
        GT -> (blankpat,) <$> [|GT|]
        EQ -> do
          namesa <- sequence $ take (argnuma-1) $ mkArgNames "a"
          namesb <- sequence $ take (argnumb-1) $ mkArgNames "b"
          let pat = TupP [ ConP namea $ WildP : map VarP namesa
                         , ConP nameb $ WildP : map VarP namesb ]
          (pat,) <$> [|mconcat $(listE $
                  zipWith joinExpr (map varE namesa) (map varE namesb) )|]
      return $ Match pat (NormalB e) []
    joinExpr :: Q Exp -> Q Exp -> Q Exp
    joinExpr a b = [|$a `compare` $b|]
  
-- |Obtains the type variable binders and constructors for a given data type.
getBindersAndCons :: Name -> Q ([TyVarBndr], [Con])
getBindersAndCons name = do
  info <- reify name
  case info of
    TyConI (DataD _ _ dTyVarBndrs dCons _) -> return (dTyVarBndrs, dCons)
    _ -> fail $ "Could not get constructors for type " ++ show name ++
                    "; got: " ++ show info
                    
-- |Determines the name and argument count of a given constructor.
conNameAndArgNum :: Con -> Q (Name,Int)
conNameAndArgNum con = case con of
  NormalC cname args -> return (cname,length args)
  _ -> fail "Can't handle non-normal data constructor!"

-- |Creates a list of argument names.
mkArgNames :: String -> [Q Name]
mkArgNames baseName = map (newName . (baseName ++)) suffixList
  -- TODO: Consider eliminating the suffix list
  where suffixList = map show [(1::Int)..]

-- |Creates an unconstrained type for typeclass instances for the given type
--  name.  This type creates a series of type variables as per @mkArgNames@
--  to use as the type arguments to the type.
mkInstanceType :: Name -> Q Type
mkInstanceType name = do
  dTyVarBndrs <- fst <$> getBindersAndCons name
  foldl appT (conT name) $ map ((varT =<<) . snd) $ zip dTyVarBndrs $ mkArgNames "t"
