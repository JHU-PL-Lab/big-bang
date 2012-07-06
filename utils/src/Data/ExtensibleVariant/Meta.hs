{-# LANGUAGE      TemplateHaskell
                #-}

{-|
  Metaprogramming facilities used by the Data.ExtensibleVariant module.
-}
module Data.ExtensibleVariant.Meta
( xvArities
, xvDecls
, opDecls
, containDecls
, xvConstrFamilyDecls
) where

import Language.Haskell.TH.Syntax

-- |The arities for which variant types are defined.
xvArities :: [Int]
xvArities = [1..4]

-- |A function to produce declarations for the k-ary variant type.
xvDecls :: Int -> [Dec]
xvDecls n = [declXvType n, declXvEq n, declXvOrd n, declXvShow n
            ,declXvDisplay n]

-- |A function to produce the @XvOp@ declarations for the k-ary variant type.
opDecls :: Int -> [Dec]
opDecls n = [declXvOp n]

-- |A function to produce the @:<<@ declarations for the ith position of
--  the j-ary variant type.
containDecls :: Int -> Int -> [Dec]
containDecls i j = [declXvContain i j]

-- |A function to produce the @:|:@ declaration for the k-ary variant type.
xvConstrFamilyDecls :: Int -> [Dec]
xvConstrFamilyDecls k =
  [TySynInstD xvExtendOp
    [appNArgs (k-1) $ ConT $ xvName (k-1), VarT $ mkName "xvp"] $
    AppT (appNArgs (k-1) $ ConT $ xvName k) $ VarT $ mkName "xvp"]
  where appNArgs n t = foldl AppT t $ map VarT $ xvTvnames n

-- |Creates the declaration for the k-ary variant type itself.
declXvType :: Int -> Dec
declXvType n =
  DataD [] name tvbinds constrs []
  where name = xvName n
        tvnames = xvTvnames n
        tvbinds = map PlainTV tvnames
        makeConstr i =
          NormalC (xvConstrName n i) $
            [(NotStrict,AppT (VarT $ tvnames !! (i-1)) $
              foldl AppT (ConT name) (map VarT tvnames))]
        constrs = map makeConstr [1..n]

-- |Creates a typeclass instance declaration of one method for a k-ary variant
--  type.
declXvTypeclassInstance :: String -> String -> [Clause] -> Int -> Dec
declXvTypeclassInstance scname sfname clauses n =
  InstanceD preds typ [impl]
  where name = xvName n
        tvnames = xvTvnames n
        preds = map makePred [1..n]
        cname = mkName scname
        fname = mkName sfname
        makePred i = ClassP cname $
            [AppT (VarT $ tvnames !! (i-1)) $ foldl AppT (ConT name) $
                map VarT tvnames]
        typ = AppT (ConT cname) $ foldl AppT (ConT name) $ map VarT tvnames
        impl = FunD fname clauses
 
-- |Creates the @Eq@ typeclass instance declaration for the k-ary variant type.
declXvEq :: Int -> Dec
declXvEq n = declXvTypeclassInstance "Eq" "==" clauses n
  where clauses = (map makeClause [1..n]) ++
                  (if n == 1 then [] else [defaultClause])
        makeClause i =
            Clause [ConP constrName $ [VarP x], ConP constrName $ [VarP y]]
            (NormalB $ AppE (AppE (VarE $ mkName "==") (VarE x)) (VarE y))
            []
          where x = mkName "x"
                y = mkName "y"
                constrName = xvConstrName n i
        defaultClause =
            Clause [WildP,WildP] (NormalB $ ConE $ mkName "False") []

-- |Creates the @Ord@ typeclass instance declaration for the k-ary variant type.
declXvOrd :: Int -> Dec
declXvOrd n = declXvTypeclassInstance "Ord" "compare" clauses n
  where clauses = map makeClause [(a,b) | a <- [1..n], b <- [1..n]]
        makeClause (i,j) =
            case compare i j of
              EQ -> eqClause
              LT -> conClause "LT"
              GT -> conClause "GT"
          where x = mkName "x"
                y = mkName "y"
                constrNameI = xvConstrName n i
                constrNameJ = xvConstrName n j
                eqClause =
                  Clause [ConP constrNameI $ [VarP x]
                         ,ConP constrNameJ $ [VarP y]]
                    (NormalB $ AppE (AppE (VarE $ mkName "compare")
                                 (VarE x)) (VarE y)) []
                conClause nm =
                  Clause [ConP constrNameI $ [WildP]
                         ,ConP constrNameJ $ [WildP]]
                    (NormalB $ ConE $ mkName nm) []

-- |Creates the @Show@ typeclass instance declaration for the k-ary variant type.
declXvShow :: Int -> Dec
declXvShow n = declXvTypeclassInstance "Show" "show" clauses n
  where clauses = map makeClause [1..n]
        makeClause i =
            Clause [ConP constrName $ [VarP x]]
            (NormalB $ AppE (AppE (VarE $ mkName "++")
                        (AppE
                            (AppE (VarE $ mkName "++") $
                                LitE $ StringL $ showName constrName ++ " (")
                            (AppE (VarE $ mkName "show") $ VarE x)
                        )) $ LitE $ StringL $ ")")
            []
          where x = mkName "x"
                constrName = xvConstrName n i

-- |Creates the @Display@ typeclass instance declaration for the k-ary variant
--  type.
declXvDisplay :: Int -> Dec
declXvDisplay n = declXvTypeclassInstance "Display" "makeDoc" clauses n
  where clauses = map makeClause [1..n]
        makeClause i =
            Clause [ConP constrName $ [VarP x]]
            (NormalB $ AppE (VarE $ mkName "makeDoc") $ VarE x) []
          where x = mkName "x"
                constrName = xvConstrName n i

-- |Creates the @XvOp@ typeclass instance declaration for the k-ary variant type.
declXvOp :: Int -> Dec
declXvOp n =
  InstanceD preds typ [impl]
  where name = xvName n
        tvnames = xvTvnames n
        preds = map makePred [1..n]
        result = mkName "result"
        op = mkName "op"
        makePred i = ClassP (mkName "XvPart") $
            [ VarT $ op
            , VarT $ tvnames !! (i-1)
            , foldl AppT (ConT name) $ map VarT tvnames
            , VarT result ]
        typ = AppT (AppT (AppT (ConT $ mkName "XvOp") $ VarT op) $
                foldl AppT (ConT name) $
                    map VarT tvnames) $ VarT result
        xv = mkName "xv"
        impl = FunD (mkName "xvop") $
            [Clause [VarP op, VarP xv] (NormalB $ CaseE (VarE xv) matches) []]
        matches = map makeMatch [1..n]
        p = mkName "p"
        makeMatch i = Match (ConP (xvConstrName n i) [VarP p])
                        (NormalB $ AppE (AppE (VarE $ mkName "xvpart") $
                            VarE op) $ VarE p) []

-- |Creates the @:<<@ typeclass instance declarations for the k-ary variant
--  type.
declXvContain :: Int -> Int -> Dec
declXvContain i j =
  InstanceD [] typ [inj,prj]
  where name = xvName j
        tvnames = xvTvnames j
        typ = AppT (AppT (ConT $ mkName ":<<") $ VarT $ tvnames !! (i-1)) $
                foldl AppT (ConT name) $ map VarT tvnames
        inj = FunD (mkName "inj") $
            [Clause [] (NormalB $ ConE $ xvConstrName j i) []]
        prjClauseGood = Clause [ConP (xvConstrName j i) [VarP $ mkName "x"]]
                (NormalB $ AppE (ConE $ mkName "Just") $ VarE $ mkName "x") []
        prjClauseBad = Clause [WildP] (NormalB $ ConE $ mkName "Nothing") [] 
        prj = FunD (mkName "prj") $
            prjClauseGood : (if j > 1 then [prjClauseBad] else [])

-- |Creates the name for the k-ary variant type.
xvName :: Int -> Name
xvName n = mkName $ "Xv" ++ show n

-- |Creates type variable names for a k-ary variant type.
xvTvnames :: Int -> [Name]
xvTvnames n = map (mkName . ("p" ++) . show) [1..n]

-- |Creates the ith constructor name for a k-ary variant type.
xvConstrName :: Int -> Int -> Name
xvConstrName n i = mkName $ "Xv" ++ show n ++ "p" ++ show i

-- |Names the infix extension operator
xvExtendOp :: Name
xvExtendOp = mkName ":||"
