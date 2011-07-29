module Language.BigBang.Render.PrettyPrint
( pretty
) where

import qualified Language.BigBang.Ast as A
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.UtilTypes
import Text.PrettyPrint

-- |Defines the indentation used by the pretty printer
indentSize :: Int
indentSize = 4

-- |Creates a textual representation of the provided Big Bang AST.
pretty :: A.Expr -> String
pretty a = render $ pp a

-- |Creates a document representation of the provided Big Bang AST.
pp :: A.Expr -> Doc
pp a = case a of
    A.Var i -> text $ unIdent i
    A.Label n e -> char '`' <> (text $ unLabelName n) <+> pp e
    A.Onion e1 e2 -> pp e1 <+> char '&' <+> pp e2
    A.Func i e -> parens $
            text "fun" <+> (text $ unIdent i) <+> text "->" <+> pp e
    A.Appl e1 e2 -> pp e1 <+> pp e2
    A.PrimInt i -> integer i
    A.PrimChar c -> char c
    A.PrimUnit -> parens empty
    A.Case e brs -> text "case" <+> pp e <+> text "of" <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map ppBranch brs)
            $+$ text "}"
    {-
       TODO: deal with the fact that the following isn't actually code
       options include:
           * errors on ASTs containing builtin nodes
           * namespacing trick (e.g., Plus translates to
               "Language.BigBang.Builtins.([+]) e1 e2"
    -}
    A.Plus e1 e2 -> pp e1 <+> text "[+]" <+> pp e2
    A.Minus e1 e2 -> pp e1 <+> text "[-]" <+> pp e2
    A.Equal e1 e2 -> pp e1 <+> text "[=]" <+> pp e2
  where ppBranch (chi,e) =
            (case chi of
                A.ChiPrim p -> ppPrimType p
                A.ChiLabel n i ->
                    char '`' <> (text $ unLabelName n) <+> (text $ unIdent i)
                A.ChiFun -> text "fun"
                A.ChiTop -> text "_"
            ) <+> text "->" <+> pp e
        ppPrimType p =
            case p of
                T.PrimInt -> text "int"
                T.PrimChar -> text "char"
                T.PrimUnit -> text "unit"
