{-# LANGUAGE      TemplateHaskell
                #-}

{-|
  Metaprogramming facilities used by the Utils.Language.Ast module.
-}
module Utils.Language.AstMeta
( astArities
, astDecls
, opDecls
, wrapDecls
) where

import Language.Haskell.TH.Syntax

-- |The arities for which AST types are defined.
astArities :: [Int]
astArities = [1..4]

-- |A function to produce declarations for the k-ary AST type.
astDecls :: Int -> [Dec]
astDecls n = [declAstType n, declAstEq n, declAstOrd n, declAstShow n
             ,declAstDisplay n]

-- |A function to produce the @AstOp@ declarations for the k-ary AST type.
opDecls :: Int -> [Dec]
opDecls n = [declAstOp n]

-- |A function to produce the @AstWrap@ declarations for the ith position of
--  the j-ary AST type.
wrapDecls :: Int -> Int -> [Dec]
wrapDecls i j = [declAstWrap i j]

-- |Creates the declaration for the k-ary AST type itself.
declAstType :: Int -> Dec
declAstType n =
  DataD [] name tvbinds constrs []
  where name = astName n
        tvnames = astTvnames n
        tvbinds = map PlainTV tvnames
        makeConstr i =
          NormalC (astConstrName n i) $
            [(NotStrict,AppT (VarT $ tvnames !! (i-1)) $
              foldl AppT (ConT name) (map VarT tvnames))]
        constrs = map makeConstr [1..n]

-- |Creates a typeclass instance declaration of one method for a k-ary AST
--  type.
declAstTypeclassInstance :: String -> String -> [Clause] -> Int -> Dec
declAstTypeclassInstance scname sfname clauses n =
  InstanceD preds typ [impl]
  where name = astName n
        tvnames = astTvnames n
        preds = map makePred [1..n]
        cname = mkName scname
        fname = mkName sfname
        makePred i = ClassP cname $
            [AppT (VarT $ tvnames !! (i-1)) $ foldl AppT (ConT name) $
                map VarT tvnames]
        typ = AppT (ConT cname) $ foldl AppT (ConT name) $ map VarT tvnames
        impl = FunD fname clauses
 
-- |Creates the @Eq@ typeclass instance declaration for the k-ary AST type.
declAstEq :: Int -> Dec
declAstEq n = declAstTypeclassInstance "Eq" "==" clauses n
  where clauses = (map makeClause [1..n]) ++
                  (if n == 1 then [] else [defaultClause])
        makeClause i =
            Clause [ConP constrName $ [VarP x], ConP constrName $ [VarP y]]
            (NormalB $ AppE (AppE (VarE $ mkName "==") (VarE x)) (VarE y))
            []
          where x = mkName "x"
                y = mkName "y"
                constrName = astConstrName n i
        defaultClause =
            Clause [WildP,WildP] (NormalB $ ConE $ mkName "False") []

-- |Creates the @Ord@ typeclass instance declaration for the k-ary AST type.
declAstOrd :: Int -> Dec
declAstOrd n = declAstTypeclassInstance "Ord" "compare" clauses n
  where clauses = map makeClause [(a,b) | a <- [1..n], b <- [1..n]]
        makeClause (i,j) =
            case compare i j of
              EQ -> eqClause
              LT -> conClause "LT"
              GT -> conClause "GT"
          where x = mkName "x"
                y = mkName "y"
                constrNameI = astConstrName n i
                constrNameJ = astConstrName n j
                eqClause =
                  Clause [ConP constrNameI $ [VarP x]
                         ,ConP constrNameJ $ [VarP y]]
                    (NormalB $ AppE (AppE (VarE $ mkName "compare")
                                 (VarE x)) (VarE y)) []
                conClause nm =
                  Clause [ConP constrNameI $ [WildP]
                         ,ConP constrNameJ $ [WildP]]
                    (NormalB $ ConE $ mkName nm) []

-- |Creates the @Show@ typeclass instance declaration for the k-ary AST type.
declAstShow :: Int -> Dec
declAstShow n = declAstTypeclassInstance "Show" "show" clauses n
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
                constrName = astConstrName n i

-- |Creates the @Display@ typeclass instance declaration for the k-ary AST
--  type.
declAstDisplay :: Int -> Dec
declAstDisplay n = declAstTypeclassInstance "Display" "makeDoc" clauses n
  where clauses = map makeClause [1..n]
        makeClause i =
            Clause [ConP constrName $ [VarP x]]
            (NormalB $ AppE (VarE $ mkName "makeDoc") $ VarE x) []
          where x = mkName "x"
                constrName = astConstrName n i

-- |Creates the @AstOp@ typeclass instance declaration for the k-ary AST type.
declAstOp :: Int -> Dec
declAstOp n =
  InstanceD preds typ [impl]
  where name = astName n
        tvnames = astTvnames n
        preds = map makePred [1..n]
        result = mkName "result"
        op = mkName "op"
        makePred i = ClassP (mkName "AstStep") $
            [ VarT $ op
            , VarT $ tvnames !! (i-1)
            , foldl AppT (ConT name) $ map VarT tvnames
            , VarT result ]
        typ = AppT (AppT (AppT (ConT $ mkName "AstOp") $ VarT op) $
                foldl AppT (ConT name) $
                    map VarT tvnames) $ VarT result
        ast = mkName "ast"
        impl = FunD (mkName "astop") $
            [Clause [VarP op, VarP ast] (NormalB $ CaseE (VarE ast) matches) []]
        matches = map makeMatch [1..n]
        p = mkName "p"
        makeMatch i = Match (ConP (astConstrName n i) [VarP p])
                        (NormalB $ AppE (AppE (VarE $ mkName "aststep") $
                            VarE op) $ VarE p) []

-- |Creates the @AstWrap@ typeclass instance declarations for the k-ary AST
--  type.
declAstWrap :: Int -> Int -> Dec
declAstWrap i j =
  InstanceD [] typ [impl]
  where name = astName j
        tvnames = astTvnames j
        typ = AppT (AppT (ConT $ mkName "AstWrap") $ VarT $ tvnames !! (i-1)) $
                foldl AppT (ConT name) $ map VarT tvnames
        impl = FunD (mkName "astwrap") $
            [Clause [] (NormalB $ ConE $ astConstrName j i) []]

-- |Creates the name for the k-ary AST type.
astName :: Int -> Name
astName n = mkName $ "Ast" ++ show n

-- |Creates type variable names for a k-ary AST type.
astTvnames :: Int -> [Name]
astTvnames n = map (mkName . ("p" ++) . show) [1..n]

-- |Creates the ith constructor name for a k-ary AST type.
astConstrName :: Int -> Int -> Name
astConstrName n i = mkName $ "Ast" ++ show n ++ "p" ++ show i

